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

open Lang_builtins

open Prometheus

let log = Log.make ["prometheus"]

type metric = [
  | `Counter of Counter.t
  | `Gauge of Gauge.t
  | `Summary of Summary.t
]

let metric_name = function
  | `Counter -> "counter"
  | `Gauge -> "gauge"
  | `Summary -> "summary"

let metrics : (string,metric) Hashtbl.t = Hashtbl.create 0

let metric_proto = [
  "help",Lang.string_t,None,Some "Help of the metric";
  "namespace",Lang.string_t,Some (Lang.string ""),Some "namespace of the metric";
  "subsystem",Lang.string_t,Some (Lang.string ""),Some "subsystem of the metric";
  "",Lang.string_t,None,Some "Name of the metric"
]


let add_metric metric_type fn =
  let metric_name = metric_name metric_type in
  add_builtin ("prometheus." ^ metric_name ^ ".register")
    ~cat:Interaction
    ~descr:("Register a prometheus " ^ metric_name)
    metric_proto
    Lang.unit_t
    (fun p ->
      let help =
        Lang.to_string (List.assoc "help" p)
      in
      let opt_v n =
        match Lang.to_string (List.assoc n p) with
          | s when s = "" -> None
          | v -> Some v
      in
      let namespace = opt_v "namespace" in
      let subsystem = opt_v "subsystem" in
      let name = Lang.to_string (List.assoc "" p) in
      let metric =
        fn ~help ?namespace ?subsystem name
      in
      Hashtbl.add metrics name metric;
      Lang.unit)

let () =
  add_metric `Counter (fun ~help ?namespace ?subsystem name ->
                `Counter (Counter.v ~help ?namespace ?subsystem name));
  add_metric `Gauge (fun ~help ?namespace ?subsystem name ->
                `Gauge (Gauge.v ~help ?namespace ?subsystem name));
  add_metric `Summary (fun ~help ?namespace ?subsystem name ->
                `Summary (Summary.v ~help ?namespace ?subsystem name))

let () =
  add_builtin "prometheus.counter.inc"
    ~cat:Interaction
    ~descr:"Increate a counter"
    ["",Lang.string_t,None,Some "Name of the metric";
     "",Lang.float_t,None,Some "Amount to increase by"]
    Lang.unit_t
    (fun p ->
      let name =
        Lang.to_string (Lang.assoc "" 1 p)
      in
      let v =
        Lang.to_float (Lang.assoc "" 2 p)
      in
      begin
       match Hashtbl.find metrics name with
         | `Counter c ->
             Counter.inc c v
         | _ ->
             log#severe "Metric %s is not a counter!" name
         | exception Not_found ->
             log#severe "Count not find metric %s!" name
      end;
      Lang.unit)

let () =
  add_builtin "prometheus.gauge.set"
    ~cat:Interaction
    ~descr:"Set a gauge"
    ["",Lang.string_t,None,Some "Name of the metric";
     "",Lang.float_t,None,Some "Value to be set"]
    Lang.unit_t
    (fun p ->
      let name =
        Lang.to_string (Lang.assoc "" 1 p)
      in
      let v =
        Lang.to_float (Lang.assoc "" 2 p)
      in
      begin
       match Hashtbl.find metrics name with
         | `Gauge g ->
             Gauge.set g v
         | _ ->
             log#severe "Metric %s is not a gauge!" name
         | exception Not_found ->
             log#severe "Count not find metric %s!" name
      end;
      Lang.unit)

let () =
  add_builtin "prometheus.summary.observe"
    ~cat:Interaction
    ~descr:"Add an observation to a summary metric"
    ["",Lang.string_t,None,Some "Name of the metric";
     "",Lang.float_t,None,Some "Value to be observed"]
    Lang.unit_t
    (fun p ->
      let name =
        Lang.to_string (Lang.assoc "" 1 p)
      in
      let v =
        Lang.to_float (Lang.assoc "" 2 p)
      in
      begin
       match Hashtbl.find metrics name with
         | `Summary s ->
             Summary.observe s v
         | _ ->
             log#severe "Metric %s is not a summary!" name
         | exception Not_found ->
             log#severe "Count not find metric %s!" name
      end;
      Lang.unit)
