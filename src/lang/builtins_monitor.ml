(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

let add_task fn =
  Duppy.Task.add Tutils.scheduler {
    Duppy.Task.
    priority = Tutils.Non_blocking;
    events   = [ `Delay 0. ];
    handler  = (fun _ ->
      fn ();
      [])
  }

let () =
  let kind = Lang.univ_t 1 in
  let content_kind_t =
    Lang.tuple_t [Lang.string_t;Lang.string_t;Lang.string_t]
  in
  let stype_t = Lang.string_t in
  let clock_sync_mode_t = Lang.string_t in
  let get_ready_t = Lang.fun_t [
    false,"stype",stype_t;
    false,"is_output",Lang.bool_t;
    false,"id",Lang.string_t;
    false,"content_kind",content_kind_t;
    false,"clock_id",Lang.string_t;
    false,"clock_sync_mode",clock_sync_mode_t
  ] Lang.unit_t in
  let get_ready = Lang.val_cst_fun [
    "stype",stype_t,None;
    "is_output",Lang.bool_t,None;
    "id",Lang.string_t,None;
    "content_kind",content_kind_t,None;
    "clock_id",Lang.string_t,None;
    "clock_sync_mode",clock_sync_mode_t,None
  ] Lang.unit in
  let leave_t = Lang.fun_t [] Lang.unit_t in
  let leave = Lang.val_cst_fun [] Lang.unit in
  let frame_metadata_t =
    Lang.product_t Lang.int_t Lang.metadata_t
  in
  let get_frame_t = Lang.fun_t [
    false,"start_time",Lang.float_t;
    false,"start_position",Lang.int_t;
    false,"end_time",Lang.float_t;
    false,"end_position",Lang.int_t;
    false,"is_partial",Lang.bool_t;
    false,"metadata",Lang.list_t frame_metadata_t
  ] Lang.unit_t in
  let get_frame = Lang.val_cst_fun [
    "start_time",Lang.float_t,None;
    "start_position",Lang.int_t,None;
    "end_time",Lang.float_t,None;
    "end_position",Lang.int_t,None;
    "is_partial",Lang.bool_t,None;
    "metadata",Lang.list_t frame_metadata_t,None
  ] Lang.unit in
  let after_output_t = Lang.fun_t [] Lang.unit_t in
  let after_output = Lang.val_cst_fun [] Lang.unit in
  Lang_builtins.add_builtin "source.monitor"
    [ "get_ready", get_ready_t, Some get_ready,
      Some "Callback exeuted when the source gets ready. \
            `content_kind` is a triplet of elements of the form: \
            `\"variable\"` or `\"<n>\"` where `<n>` is a fixed number. \
            Each triplet represent the kind of, resp. `audio`, `video` and `midi` \
            content that the source produces. `stype` is one of: \
            `\"fallible\"` or `\"infallible\"`, `clock_sync_mode` is one of: \
            `\"default\"`, `\"synced_wallclock\"`, `\"unsynced_wallclock\"` or \
            `\"self_synced\"`";
      "leave", leave_t, Some leave,
      Some "Callback executed when the source is shutdown";
      "get_frame", get_frame_t, Some get_frame,
      Some "Callback executed when the source is asked to produce data. \
            start/end position are in ticks";
      "after_output", after_output_t, Some after_output,
      Some "Callback executed when a whole output round has finished";
      "", Lang.source_t kind, None, None ]
    Lang.unit_t
    ~cat:Lang_builtins.Liq
    ~descr:"Monitor a source's internals"
    (fun p ->
       let get_ready =
         Lang.to_fun ~t:Lang.unit_t (List.assoc "get_ready" p)
       in
       let get_ready ~stype ~is_output ~id ~content_kind
                     ~clock_id ~clock_sync_mode =
         add_task (fun () ->
           let stype = match stype with
             | Source.Fallible -> Lang.string "fallible"
             | Source.Infallible -> Lang.string "infallible"
           in
           let is_output = Lang.bool is_output in
           let id = Lang.string id in
           let content_kind =
             let rec string_of_kind cur = function
               | Frame.Variable -> Lang.string "variable"
               | Frame.Zero -> Lang.string (Printf.sprintf "%d" cur)
               | Frame.Succ v -> string_of_kind (cur+1) v
             in
             Lang.tuple [
               string_of_kind 0 content_kind.Frame.audio;
               string_of_kind 0 content_kind.Frame.video;
               string_of_kind 0 content_kind.Frame.midi
             ]
           in
           let clock_id = Lang.string clock_id in
           let clock_sync_mode = match clock_sync_mode with
             | `Auto -> Lang.string "auto"
             | `CPU -> Lang.string "cpu"
             | `None -> Lang.string "none"
           in
           ignore(get_ready [
             "stype",stype;
             "is_output",is_output;
             "id",id;
             "content_kind",content_kind;
             "clock_id",clock_id;
             "clock_sync_mode",clock_sync_mode
           ]));
       in
       let leave =
         Lang.to_fun ~t:Lang.unit_t (List.assoc "leave" p)
       in
       let leave () =
         add_task (fun () ->
           ignore(leave [])
         )
       in
       let get_frame =
         Lang.to_fun ~t:Lang.unit_t (List.assoc "get_frame" p)
       in
       let get_frame ~start_time ~end_time ~start_position ~end_position
                     ~is_partial ~metadata =
         add_task (fun () ->
           ignore(get_frame [
             "start_time",Lang.float start_time;
             "start_position",Lang.int start_position;
             "end_time",Lang.float end_time;
             "end_position",Lang.int end_position;
             "is_partial",Lang.bool is_partial;
             "metadata",Lang.list ~t:frame_metadata_t
                (List.map (fun (pos,m) ->
                  Lang.product (Lang.int pos) (Lang.metadata m)) metadata)
           ])
         )
       in
       let after_output =
         Lang.to_fun ~t:Lang.unit_t (List.assoc "after_output" p)
       in
       let after_output () =
         add_task (fun () ->
           ignore(after_output [])
         )
       in
       let watcher = {Source.
         get_ready;leave;get_frame;after_output
       } in
       let s = Lang.to_source (List.assoc "" p) in
       s#add_watcher watcher;
       Lang.unit)
