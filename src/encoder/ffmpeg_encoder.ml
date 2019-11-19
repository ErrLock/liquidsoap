(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** FFMPEG encoder *)

open FFmpeg

module Resampler = Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)
module Scaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame)

let log = Ffmpeg_config.log

type handler = {
  output: Avutil.output Avutil.container;
  audio_stream: (Avutil.output, Avutil.audio) Av.stream option;
  video_stream: (Avutil.output, Avutil.video) Av.stream option;
  converter: (Swresample.FltPlanarBigArray.t, Swresample.Frame.t) Swresample.ctx option;
  scaler: (Swscale.BigArray.t, Swscale.Frame.t) Swscale.ctx option
}
(* Convert ffmpeg-specific options. *)
let convert_options opts =
  let convert name fn =
    match Hashtbl.find_opt opts name with
      | None -> ()
      | Some v -> Hashtbl.replace opts name (fn v)
  in
  convert "sample_fmt" (function
    | `String fmt ->
        `Int (FFmpeg.Avutil.Sample_format.(get_id (find fmt)))
    | _ -> assert false);
  convert "channel_layout" (function
    | `String layout ->
        `Int (FFmpeg.Avutil.Channel_layout.(get_id (find layout)))
    | _ -> assert false)

let encoder ffmpeg meta =
  let short_name = ffmpeg.Ffmpeg_format.format in
  let format =
    match Av.Format.guess_output_format ~short_name () with
      | None -> failwith "No format for filename!"
      | Some f -> f
  in
  let audio_codec =
    Utils.maybe
      Avcodec.Audio.find_encoder
        ffmpeg.Ffmpeg_format.audio_codec
  in
  let video_codec =
    Utils.maybe
      Avcodec.Video.find_encoder
        ffmpeg.Ffmpeg_format.video_codec
  in
  let src_freq = Frame.audio_of_seconds 1. in
  let channels = Lazy.force Frame.audio_channels in
  if channels > 0 && audio_codec = None then
    failwith "Audio codec required when channels > 0";
  let vchans =
    if video_codec = None then 0 else 1
  in
  let src_channels =
    match channels with
      | 1 -> `Mono
      | 2 -> `Stereo
      | _ -> failwith "%ffmpeg encoder only supports mono or stereo audio for now!"
  in
  let dst_freq = 
    Lazy.force ffmpeg.Ffmpeg_format.samplerate
  in
  let dst_channels =
    match ffmpeg.Ffmpeg_format.channels with
      | 1 -> `Mono
      | 2 -> `Stereo
      | _ -> failwith "%ffmpeg encoder only supports mono or stereo audio for now!"
  in
  let video_width =
    Lazy.force Frame.video_width
  in
  let video_height =
    Lazy.force Frame.video_height
  in
  let buf = Strings.Mutable.empty () in
  let options = Hashtbl.copy ffmpeg.Ffmpeg_format.options in
  convert_options options;
  let make () =
    let opts = Hashtbl.create 10 in
    let converter =
      Utils.maybe (fun audio_codec ->
        let audio_opts =
          Av.mk_audio_opts ~channels:ffmpeg.Ffmpeg_format.channels
                           ~sample_rate:(Lazy.force ffmpeg.Ffmpeg_format.samplerate)
                           ()
        in
        Hashtbl.iter (Hashtbl.add opts) audio_opts;
        let out_sample_format =
          Avcodec.Audio.find_best_sample_format audio_codec `Dbl
        in
        Resampler.create ~out_sample_format
          src_channels src_freq
          dst_channels dst_freq) audio_codec
    in
    let scaler =
      Utils.maybe (fun video_codec ->
        let pixel_format =
            Avcodec.Video.find_best_pixel_format
              video_codec `Yuv420p
        in
        let video_opts =
          Av.mk_video_opts ~pixel_format
            ~size:(video_width,video_height)
           ()
        in
        Hashtbl.iter (Hashtbl.add opts) video_opts;
        Scaler.create []
          video_width video_height `Yuv420p
          video_width video_height pixel_format) video_codec
    in
    Hashtbl.iter (Hashtbl.add opts) options;
    let write str ofs len =
      Strings.Mutable.add_subbytes buf str ofs len;
      len
    in
    let output =
      Av.open_output_stream ~opts write format
    in
    let audio_stream =
      Utils.maybe (fun audio_codec ->
        Av.new_audio_stream ~opts ~codec:audio_codec output) audio_codec
    in
    let video_stream =
      Utils.maybe (fun video_codec ->
        Av.new_video_stream ~opts ~codec:video_codec output) video_codec
    in
    if Hashtbl.length opts > 0 then
       failwith (Printf.sprintf "Unrecognized options: %s" 
         (Ffmpeg_format.string_of_options opts));
    {output; audio_stream; video_stream; converter; scaler}
  in
  let h = ref (make ()) in
  let encode frame start len =
    let content =
      Frame.content_of_type frame start
        {Frame.
          audio = channels;
          video = vchans;
          midi = 0;
        }
    in
    ignore(Utils.maybe (fun _ ->
      let pcm = content.Frame.audio in
      let aframe =
        Resampler.convert (Utils.get_some !h.converter) pcm
      in
      Av.write_frame (Utils.get_some !h.audio_stream) aframe) audio_codec);
    ignore(Utils.maybe (fun _ ->
      let vstart = Frame.video_of_master start in
      let vlen = Frame.video_of_master len in
      let vbuf = content.Frame.video in
      let vbuf = vbuf.(0) in
      for i = vstart to vstart+vlen-1 do
        let f = Video.get vbuf i in
        let y,u,v = Image.YUV420.data f in
        let sy = Image.YUV420.y_stride f in
        let s = Image.YUV420.uv_stride f in
        let vdata = [|(y,sy);(u,s);(v,s)|] in
        let vframe =
          Scaler.convert (Utils.get_some !h.scaler) vdata
        in
        Av.write_frame (Utils.get_some !h.video_stream) vframe
      done) video_codec);
    Strings.Mutable.flush buf
  in
  let insert_metadata m =
    Av.close !h.output;
    h := make ();
    let m = Hashtbl.fold (fun lbl v l ->
      (lbl,v)::l) (Meta_format.to_metadata m) []
    in
    match !h.audio_stream, !h.video_stream with
      | Some s, _ ->
          Av.set_metadata s m
      | None, Some s ->
          Av.set_metadata s m
      | _ -> ()
  in
  insert_metadata meta;
  let stop () = 
    Av.close !h.output;
    Strings.Mutable.flush buf
  in
    {
     Encoder.
      insert_metadata = insert_metadata ;
      header = Strings.empty ;
      encode = encode ;
      stop = stop
    }

let () =
  Encoder.plug#register "FFMPEG"
    (function
       | Encoder.Ffmpeg m -> Some (fun _ -> encoder m)
       | _ -> None)
