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

 (* Monitor a source's internals. *)

type metadata = (int*(string, string) Hashtbl.t) list

type watchers = {
  get_ready : stype:Source.source_t -> is_output:bool -> id:string ->
              content_kind:Frame.content_kind ->
              clock_id:string -> clock_sync_mode:Source.sync -> unit;
  leave : unit -> unit;
  seek : asked:int -> effective:int -> unit;
  is_ready : bool -> unit;
  get_frame : start_time:float -> end_time:float ->
              start_position:int -> end_position:int -> 
              is_partial:bool -> metadata:metadata -> unit;
  after_output : unit -> unit;
  abort_track : unit -> unit
}

let add_task fn =
  Duppy.Task.add Tutils.scheduler {
    Duppy.Task.
    priority = Tutils.Non_blocking;
    events   = [ `Delay 0. ];
    handler  = (fun _ ->
      fn ();
      [])
  }

class monitor ~kind ~watchers s =
object
  inherit Source.operator ~name:"monitor" kind [s] 

  method stype = s#stype

  method self_sync = s#self_sync
  
  method get_ready ?dynamic activation =
    s#get_ready ?dynamic activation;
    let clock = Clock.get s#clock in
    watchers.get_ready ~stype:s#stype ~is_output:s#is_output
                       ~id:s#id ~content_kind:s#kind
                       ~clock_id:clock#id ~clock_sync_mode:clock#sync_mode

  method leave ?dynamic caller =
    s#leave ?dynamic caller;
    watchers.leave ()

  method seek asked =
    let effective = s#seek asked in
    watchers.seek ~asked ~effective;
    effective

  val mutable last_ready = None

  method is_ready =
    let ret = s#is_ready in
    begin
      match ret, last_ready with
        | x, Some y when x = y -> ()
        | _ ->
           last_ready <- Some ret;
           watchers.is_ready ret
    end;
    ret

  method private get_frame frame =
    let start_time =
      Unix.gettimeofday ()
    in
    let start_position =
      Frame.position frame
    in
    s#get frame;
    let end_time =
      Unix.gettimeofday ()
    in
    let end_position =
      Frame.position frame
    in
    let is_partial =
      Frame.is_partial frame
    in
    let metadata =
      List.filter (fun (pos,_) ->
        start_position <= pos) (Frame.get_all_metadata frame)
    in
    watchers.get_frame
      ~start_time ~start_position
      ~end_time ~end_position ~is_partial ~metadata   

  method after_output =
    s#after_output;
    watchers.after_output ()

  method abort_track =
    s#abort_track;
    watchers.abort_track ()

  method remaining = s#remaining
end

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
  let seek_t = Lang.fun_t [
    false,"asked",Lang.int_t;
    false,"effective",Lang.int_t
  ] Lang.unit_t in
  let seek = Lang.val_cst_fun [
    "asked",Lang.int_t,None;
    "effective",Lang.int_t,None
  ] Lang.unit in
  let is_ready_t = Lang.fun_t [
    false,"",Lang.bool_t
  ] Lang.unit_t in
  let is_ready = Lang.val_cst_fun [
    "",Lang.bool_t,None
  ] Lang.unit in
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
  let abort_track_t = Lang.fun_t [] Lang.unit_t in
  let abort_track = Lang.val_cst_fun [] Lang.unit in
  let after_output_t = Lang.fun_t [] Lang.unit_t in
  let after_output = Lang.val_cst_fun [] Lang.unit in
  Lang.add_operator "monitor"
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
      "seek", seek_t, Some seek,
      Some "Callback executed when the source is asked to seek to given position";
      "is_ready", is_ready_t, Some is_ready,
      Some "Callback executed when the source change its ready status";
      "get_frame", get_frame_t, Some get_frame,
      Some "Callback executed when the source is asked to produce data. \
            start/end position are in ticks";
      "abort_track", abort_track_t, Some abort_track,
      Some "Callback executed when the source is asked to abort its current track";
      "after_output", after_output_t, Some after_output,
      Some "Callback executed when a whole output round has finished";
      "", Lang.source_t kind, None, None ]
    ~category:Lang.Liquidsoap
    ~descr:"Monitor a source's internals"
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
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
       let seek =
         Lang.to_fun ~t:Lang.unit_t (List.assoc "seek" p)
       in
       let seek ~asked ~effective =
         add_task (fun () ->
           ignore(seek ["asked",Lang.int asked;"effective",Lang.int effective])
         )
       in
       let is_ready =
         Lang.to_fun ~t:Lang.unit_t (List.assoc "is_ready" p)
       in
       let is_ready b =
         add_task (fun () ->
           ignore(is_ready ["",Lang.bool b])
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
       let abort_track =
         Lang.to_fun ~t:Lang.unit_t (List.assoc "abort_track" p)
       in
       let abort_track () =
         add_task (fun () ->
           ignore(abort_track [])
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
       let watchers = {
         get_ready;leave;seek;is_ready;
         get_frame;abort_track;after_output
       } in
       let s = Lang.to_source (List.assoc "" p) in
       new monitor ~kind ~watchers s)
