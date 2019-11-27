(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

(** Decode and read metadata using ffmpeg. *)

exception End_of_file
exception No_stream

let log = Log.make ["decoder";"ffmpeg"]

(** Configuration keys for ffmpeg. *)
let mime_types =
  Dtools.Conf.list ~p:(Decoder.conf_mime_types#plug "ffmpeg")
    "Mime-types used for decoding with ffmpeg"
    ~d:["application/ffmpeg"]

let file_extensions =
  Dtools.Conf.list ~p:(Decoder.conf_file_extensions#plug "ffmpeg")
    "File extensions used for decoding with ffmpeg"
    ~d:["mp3";"mp4";"m4a";"wav";"flac";"ogg";"wma";"webm";"osb"]

module ConverterInput = FFmpeg.Swresample.Make(FFmpeg.Swresample.Frame)
module Converter = ConverterInput(FFmpeg.Swresample.FltPlanarBigArray)
module Scaler = FFmpeg.Swscale.Make (FFmpeg.Swscale.Frame) (FFmpeg.Swscale.BigArray)

module G = Generator.From_audio_video
module Buffered = Decoder.Buffered(G)

let mk_audio_decoder ~put_audio container =
  let (idx, stream, codec) =
    FFmpeg.Av.find_best_audio_stream container
  in
  let sample_freq =
    FFmpeg.Avcodec.Audio.get_sample_rate codec
  in
  let channel_layout =
    FFmpeg.Avcodec.Audio.get_channel_layout codec
  in
  let target_sample_rate =
    Lazy.force Frame.audio_rate
  in
  let in_sample_format =
    ref (FFmpeg.Avcodec.Audio.get_sample_format codec)
  in
  let mk_converter () =
    Converter.create channel_layout ~in_sample_format:!in_sample_format sample_freq
                     channel_layout target_sample_rate
  in
  let converter = ref (mk_converter ()) in
  idx, stream, fun frame gen ->
    let frame_in_sample_format =
      FFmpeg.Avutil.Audio.frame_get_sample_format frame
    in
    if !in_sample_format <> frame_in_sample_format then
     begin
      log#important "Sample format change detected!";
      in_sample_format := frame_in_sample_format;
      converter := mk_converter ();
     end;
    let content =
      Converter.convert !converter frame
    in
    put_audio gen content 0 (Audio.length content)

let mk_video_decoder ~put_video container =
  let (idx, stream, codec) =
    FFmpeg.Av.find_best_video_stream container
  in
  let pixel_format =
    FFmpeg.Avcodec.Video.get_pixel_format codec
  in
  let width =
    FFmpeg.Avcodec.Video.get_width codec
  in
  let height =
    FFmpeg.Avcodec.Video.get_height codec
  in
  let target_frame_rate =
    Lazy.force Frame.video_rate
  in
  let target_width =
    Lazy.force Frame.video_width
  in
  let target_height =
    Lazy.force Frame.video_height
  in
  let scaler =
    Scaler.create []
      width height pixel_format
      target_width target_height `Yuv420p
  in
  let time_base =
    FFmpeg.Av.get_time_base stream
  in
  let pixel_aspect =
    FFmpeg.Av.get_pixel_aspect stream
  in
  let graph =
    FFmpeg.Avfilter.init ()
  in
  let buffer =
    FFmpeg.Avfilter.create_filter
      ~args:(Printf.sprintf
        "video_size=%dx%d:pix_fmt=%s:time_base=%d/%d:pixel_aspect=%d/%d"
        width height (FFmpeg.Avutil.Pixel_format.to_string pixel_format) 
        time_base.FFmpeg.Avutil.num time_base.FFmpeg.Avutil.den
        pixel_aspect.FFmpeg.Avutil.num pixel_aspect.FFmpeg.Avutil.den)
      ~name:"buffer" graph
  in
  let fps =
    FFmpeg.Avfilter.create_filter
      ~args:(Printf.sprintf "fps=%d:start_time=0" target_frame_rate)
      ~name:"fps" graph
  in
  let sink =
    FFmpeg.Avfilter.create_filter ~name:"buffersink" graph
  in
  FFmpeg.Avfilter.link buffer 0 fps 0;
  FFmpeg.Avfilter.link fps 0 sink 0;
  FFmpeg.Avfilter.config graph;
  idx, stream, fun frame gen ->
    FFmpeg.Avfilter.write_frame buffer frame;
    let rec flush () =
      try
        let frame = FFmpeg.Avfilter.get_frame sink in
        let img =
          match Scaler.convert scaler frame with
            | [|y,sy;u,s;v,_|] ->
                Image.YUV420.make
                  target_width target_height
                  y sy u v s
            | _ -> assert false
       in
       let content = Video.single img in
       put_video gen [|content|] 0 (Video.length content);
       flush()
     with FFmpeg.Avutil.Error `Eagain -> ()
   in
   flush ()

let mk_decoder ~set_mode ~add_break ~audio ~video ~container =
  let rec read_input_frame () =
    try FFmpeg.Av.read_input_frame container with
      | FFmpeg.Avutil.Error `Invalid_data ->
         read_input_frame ()
  in
  let mode_set = ref false in
  fun gen ->
    if not !mode_set then
     begin
      match audio, video with
        | Some _, Some _ ->
            set_mode gen `Both
        | Some _, None ->
            set_mode gen `Audio
        | None, Some _ ->
            set_mode gen `Video
        | _ -> failwith "no content!"
     end;
    mode_set := true;
    match audio, video, read_input_frame () with
      | Some (idx, _, decoder), _, `Audio (i, frame) when i = idx ->
          decoder frame gen
      | _, Some (idx, _, decoder), `Video (i, frame) when i = idx ->
          decoder frame gen
      | _ -> ()
      | exception FFmpeg.Avutil.Error `Eof ->
          add_break ?sync:(Some `Drop) gen;
          raise End_of_file

let seek ~audio ~video ticks =
  let position = Frame.seconds_of_master ticks in
  let position = Int64.of_float
    (position *. 1000.)
  in
  let seek stream =
    try
      FFmpeg.Av.seek stream `Millisecond position [||];
      ticks
    with FFmpeg.Avutil.Error _ -> 0
  in
  match audio, video with
    | Some (_, s, _), _ -> seek s
    | _, Some (_, s, _) -> seek s
    | _ -> raise No_stream

let duration file =
  let container =
    FFmpeg.Av.open_input file
  in
  Tutils.finalize ~k:(fun () -> FFmpeg.Av.close container)
    (fun () ->
      let duration =
        FFmpeg.Av.get_input_duration container ~format:`Millisecond
      in
      (Int64.to_float duration) /. 1000.)

let create_decoder fname =
  let duration = duration fname in
  let remaining = ref duration in
  let m = Mutex.create () in
  let set_remaining stream frame = Tutils.mutexify m (fun () ->
    let pts = FFmpeg.Avutil.frame_pts frame in
    let {FFmpeg.Avutil.num;den} =
      FFmpeg.Av.get_time_base stream
    in
    let position =
      Int64.to_float (Int64.mul (Int64.of_int num) pts) /.
      (float den)
    in
    remaining := duration -. position) ()
  in
  let get_remaining = Tutils.mutexify m (fun () ->
    Frame.master_of_seconds !remaining)
  in
  let container =
    FFmpeg.Av.open_input fname
  in
  let audio =
    try
      let idx, stream, decoder =
        mk_audio_decoder ~put_audio:G.put_audio container
      in
      Some (idx, stream, fun frame gen ->
        set_remaining stream frame;
        decoder frame gen)
    with FFmpeg.Avutil.Error _ -> None
  in
  let video =
    try
      let idx, stream, decoder =
        mk_video_decoder ~put_video:G.put_video container
      in
      Some (idx, stream, fun frame gen ->
        set_remaining stream frame;
        decoder frame gen)
    with FFmpeg.Avutil.Error _ -> None 
  in
  let close () =
    FFmpeg.Av.close container
  in
  { Decoder.
     seek = seek ~audio ~video;
     decode = mk_decoder
       ~set_mode:G.set_mode ~add_break:G.add_break
       ~audio ~video ~container
  }, close, get_remaining

let create_file_decoder filename kind =
  let generator = G.create `Audio in
  let decoder, close, remaining =
    create_decoder filename
  in
  let remaining frame offset =
    let remaining = remaining () in
    remaining + G.length generator + Frame.position frame - offset 
  in
  Buffered.make_file_decoder ~filename ~close ~kind ~remaining decoder generator 

(* Get the type of an input container. *)
let get_type ~url container =
  let audio, descr =
    try
      let (_, _, codec) =
        FFmpeg.Av.find_best_audio_stream container
      in
      let channels =
        FFmpeg.Avcodec.Audio.get_nb_channels codec
      in
      let rate =
        FFmpeg.Avcodec.Audio.get_sample_rate codec
      in
      let codec_name =
        FFmpeg.Avcodec.Audio.string_of_id
          (FFmpeg.Avcodec.Audio.get_params_id codec)
      in
      channels, [Printf.sprintf "audio: {codec: %s, %dHz, %d channel(s)}"
                   codec_name rate channels]
     with FFmpeg.Avutil.Error _ -> 0, []
    in
    let video, descr =
     try
      let (_, _, codec) =
        FFmpeg.Av.find_best_video_stream container
      in
      let width =
        FFmpeg.Avcodec.Video.get_width codec
      in
      let height =
        FFmpeg.Avcodec.Video.get_height codec
      in
      let pixel_format =
        FFmpeg.Avutil.Pixel_format.to_string
          (FFmpeg.Avcodec.Video.get_pixel_format codec)
      in
      let codec_name =
        FFmpeg.Avcodec.Video.string_of_id
          (FFmpeg.Avcodec.Video.get_params_id codec)
      in  
      1, (Printf.sprintf "video: {codec: %s, %dx%d, %s}"
            codec_name width height pixel_format)::descr
     with FFmpeg.Avutil.Error _ -> 0, descr
    in
    if audio == 0 && video == 0 then
      failwith "No valid stream found in file.";
    log#info "ffmpeg recognizes %S as: %s."
      url (String.concat ", " (List.rev descr));
    {Frame.
       audio;
       video;
       midi  = 0 }

let get_file_type filename =
  let container =
    FFmpeg.Av.open_input filename
  in
  Tutils.finalize ~k:(fun () -> FFmpeg.Av.close container)
  (fun () -> get_type ~url:filename container)

let () =
  Decoder.file_decoders#register
  "FFMPEG"
  ~sdoc:"Use libffmpeg to decode any file \
         if its MIME type or file extension is appropriate."
  (fun ~metadata:_ filename kind ->
     if not (Decoder.test_file ~mimes:mime_types#get 
                               ~extensions:file_extensions#get
                               ~log filename) then
       None
     else
       if kind.Frame.audio = Frame.Variable ||
          kind.Frame.audio = Frame.Succ Frame.Variable ||
          if Frame.type_has_kind (get_file_type filename) kind then true else begin
            log#important
              "File %S has an incompatible number of channels."
              filename ;
            false
          end
       then
         Some (fun () -> create_file_decoder filename kind)
       else
         None)

let log = Log.make ["metadata";"ffmpeg"]

let get_tags file =
  let container =
    FFmpeg.Av.open_input file
  in
  Tutils.finalize ~k:(fun () -> FFmpeg.Av.close container)
    (fun () ->
      FFmpeg.Av.get_input_metadata container)

let () = Request.mresolvers#register "FFMPEG" get_tags

let check filename =
  match Configure.file_mime with
    | Some f -> List.mem (f filename) mime_types#get
    | None -> (try ignore (get_file_type filename) ; true with _ -> false)

let () =
  Request.dresolvers#register "FFMPEG" duration

module Make (Generator:Generator.S_Asio) =
struct
let create_decoder input =
  let read = input.Decoder.read in
  let seek_input =
    match input.Decoder.lseek with
      | None -> None
      | Some fn ->
        Some (fun len _ -> fn len)
  in
  let container =
    FFmpeg.Av.open_input_stream ?seek:seek_input read
  in
  let audio =
    try
      Some (mk_audio_decoder ~put_audio:Generator.put_audio container)
    with FFmpeg.Avutil.Error _ -> None
  in
  let video =
    try
      Some (mk_video_decoder ~put_video:Generator.put_video container)
    with FFmpeg.Avutil.Error _ -> None
  in
  { Decoder.
     seek = seek ~audio ~video;
     decode = mk_decoder
       ~set_mode:Generator.set_mode ~add_break:Generator.add_break
       ~audio ~video ~container }
end

module D_stream = Make(Generator.From_audio_video_plus)

let () =
  Decoder.stream_decoders#register
    "FFMPEG"
    ~sdoc:"Use ffmpeg/libav to decode any stream with an appropriate MIME type."
     (fun mime kind ->
        let (<:) a b = Frame.mul_sub_mul a b in
          if List.mem mime mime_types#get &&
             (* Check that it is okay to have zero video and midi,
              * and at least one audio channel. *)
             Frame.Zero <: kind.Frame.video &&
             Frame.Zero <: kind.Frame.midi &&
             kind.Frame.audio <> Frame.Zero
          then
            (* In fact we can't be sure that we'll satisfy the content
             * kind, because the stream might be mono or stereo.
             * For now, we let this problem result in an error at
             * decoding-time. Failing early would only be an advantage
             * if there was possibly another plugin for decoding
             * correctly the stream (e.g. by performing conversions). *)
            Some D_stream.create_decoder
          else
            None)

