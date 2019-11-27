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

open Lang_values
open Lang_encoders

let ffmpeg_gen params =
  let defaults =
    { Ffmpeg_format.
        format = Some "mp3" ;
        output = `Stream ;
        audio_codec = Some "libmp3lame" ;
        video_codec = None ;
        channels = 2 ;
        samplerate = Frame.audio_rate ;
        options = Hashtbl.create 0 }

  in
  List.fold_left
      (fun f ->
        function
          | ("format",{ term = String fmt; _}) when fmt = "" ->
              { f with Ffmpeg_format.format = None }
          | ("format",{ term = String fmt; _}) ->
              { f with Ffmpeg_format.format = Some fmt }
          | ("audio_codec",{ term = String c; _}) when c = "" ->
              { f with Ffmpeg_format.audio_codec = None }
          | ("audio_codec",{ term = String c; _}) ->
              { f with Ffmpeg_format.audio_codec = Some c }
          | ("video_codec",{ term = String c; _}) when c = "" ->
              { f with Ffmpeg_format.video_codec = None }
          | ("video_codec",{ term = String c; _}) ->
              { f with Ffmpeg_format.video_codec = Some c }
          | ("channels",{ term = Int i; _})
          | ("ac",{ term = Int i; _}) ->
              { f with Ffmpeg_format.channels = i }
          | ("samplerate",{ term = Int i; _})
          | ("ar",{ term = Int i; _}) ->
              { f with Ffmpeg_format.samplerate = Lazy.from_val i }
          | ("sample_fmt",{ term = String fmt; _}) ->
              Hashtbl.add f.Ffmpeg_format.options "sample_fmt"
                (`String fmt);
              f
          | ("channel_layout",{ term = String layout; _}) ->
              Hashtbl.add f.Ffmpeg_format.options "channel_layout"
                (`String layout);
              f
          | ("url",{ term = String url; _}) ->
            { f with Ffmpeg_format.output = `Url url} 
          | (k,{ term = String s; _}) ->
              Hashtbl.add f.Ffmpeg_format.options k (`String s);
              f
          | (k,{ term = Int i; _}) ->
              Hashtbl.add f.Ffmpeg_format.options k (`Int i);
              f
          | (k,{ term = Float i; _}) ->
              Hashtbl.add f.Ffmpeg_format.options k (`Float i);
              f
          | ("",{ term = Var s; _}) when String.lowercase_ascii s = "mono" ->
              { f with Ffmpeg_format.channels = 1 }
          | ("",{ term = Var s; _}) when String.lowercase_ascii s = "stereo" ->
              { f with Ffmpeg_format.channels = 2 }
          | (_,t) -> raise (generic_error t))
      defaults params

let make params =
    Encoder.Ffmpeg (ffmpeg_gen params)
