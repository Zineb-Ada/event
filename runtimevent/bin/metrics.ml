let namespace = "runtime"

let subsystem = "main"

module Counters = struct
  open Prometheus

let minor_promoted =
  let help = "Number of words promoted to the shared heap" in
  Counter.v ~help ~namespace ~subsystem "minor_promoted"

let minor_allocated =
  let help = "Number of allocated words on the minor heap" in
  Counter.v ~help ~namespace ~subsystem "minor_allocated"

let force_minor_alloc_small =
  let help = "Number of small allocated words on the minor heap" in
  Counter.v ~help ~namespace ~subsystem "force_minor_alloc_small"

let force_minor_make_vect =
  let help = "Number of vector on the minor heap" in
  Counter.v ~help ~namespace ~subsystem "force_minor_make_vect"

let force_minor_set_minor_heap_size =
  let help = "" in
  Counter.v ~help ~namespace ~subsystem "force_minor_set_minor_heap_size"

let force_minor_memprof =
  let help = "" in
  Counter.v ~help ~namespace ~subsystem "force_minor_memprof"

let request_major_alloc_shr =
  let help = "" in
  Counter.v ~help ~namespace ~subsystem "request_major_alloc_shr"

let request_major_adjust_gc_speed =
  let help = "" in
  Counter.v ~help ~namespace ~subsystem "request_major_adjust_gc_speed"

let request_minor_realloc_ref_table =
  let help = "" in
  Counter.v ~help ~namespace ~subsystem "request_minor_realloc_ref_table"

let request_minor_realloc_ephe_ref_table =
  let help = "" in
  Counter.v ~help ~namespace ~subsystem "request_minor_realloc_ephe_ref_table"

let request_minor_realloc_custom_table =
  let help = "" in
  Counter.v ~help ~namespace ~subsystem "request_minor_realloc_custom_table"

end
(* http://localhost:3333/explore *)
(* prometheus --config.file=prometheus.yml
  dune exec bin/runtimevent.exe _build/default/test/main.exe
  /Users/tarides/Desktop/Tarides-Ocaml/prometheus_eventring/grafana-9.1.7/bin/grafana-server -homepath /Users/tarides/Desktop/Tarides-Ocaml/prometheus_eventring/grafana-9.1.7 *)