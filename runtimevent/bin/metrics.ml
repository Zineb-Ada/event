let namespace = "runtime"
let subsystem = "counter"

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

let subsystem = "phases"
(* type histogram = HistogramOpts {
  Name : "runtime_phases_histogram_metric",
  Buckets : 
}  *)
(* let name = "runtime_phases_histogram_metric_bucket"  *)
(* let buckets = [] *)
(* module PH = HistogramOpts (
  struct 
    open Prometheus
    
) *)
module Phases = struct
  open Prometheus 

let major = 
  let help = "phases" in
    Gauge.v ~help ~namespace ~subsystem "major"

let minor = 
  let help = "phases" in
    Gauge.v ~help ~namespace ~subsystem "minor"
  (* Counter.v ~help ~namespace ~subsystem "" *)
  (* rate(http_request_latency_bucket)[1m]
  Buckets.v ~help ~namespace ~subsystem "explicit_gc_full_major_bucket" *)
  (* (bucket: "")
  http_response_duration_ms{quartile="0.5"} *)
  (* let help = "phases" in  *)
  (* http_request_duration_seconds_bucket{le=f} *)
end

(* explicit_gc_full_major.Observe(0.1) *)
(* histogram := promauto.NewHistogram(prometheus.HistogramOpts{
		Name:    "runtime_phases",
		Buckets: []float64{1.0, 2.0, 3.0, 4.0, 5.0},
	}) *)


(* |	EV_EXPLICIT_GC_SET explicit_gc_set
|	EV_EXPLICIT_GC_STAT
|	EV_EXPLICIT_GC_MINOR
|	EV_EXPLICIT_GC_MAJOR
|	EV_EXPLICIT_GC_FULL_MAJOR
|	EV_EXPLICIT_GC_COMPACT
|	EV_MAJOR
|	EV_MAJOR_SWEEP
|	EV_MAJOR_MARK_ROOTS
|	EV_MAJOR_MARK
|	EV_MINOR
|	EV_MINOR_LOCAL_ROOTS
|	EV_MINOR_FINALIZED
|	EV_EXPLICIT_GC_MAJOR_SLICE
|	EV_FINALISE_UPDATE_FIRST
|	EV_FINALISE_UPDATE_LAST
|	EV_INTERRUPT_REMOTE
|	EV_MAJOR_EPHE_MARK
|	EV_MAJOR_EPHE_SWEEP
|	EV_MAJOR_FINISH_MARKING
|	EV_MAJOR_GC_CYCLE_DOMAINS
|	EV_MAJOR_GC_PHASE_CHANGE
|	EV_MAJOR_GC_STW
|	EV_MAJOR_MARK_OPPORTUNISTIC
|	EV_MAJOR_SLICE
|	EV_MAJOR_FINISH_CYCLE
|	EV_MINOR_CLEAR
|	EV_MINOR_FINALIZERS_OLDIFY
|	EV_MINOR_GLOBAL_ROOTS
|	EV_MINOR_LEAVE_BARRIER
|	EV_STW_API_BARRIER
|	EV_STW_HANDLER
|	EV_STW_LEADER
|	EV_MAJOR_FINISH_SWEEPING
|	EV_MINOR_FINALIZERS_ADMIN
|	EV_MINOR_REMEMBERED_SET
|	EV_MINOR_REMEMBERED_SET_PROMOTE
|	EV_MINOR_LOCAL_ROOTS_PROMOTE
|	EV_DOMAIN_CONDITION_WAIT
|	EV_DOMAIN_RESIZE_HEAP_RESERVATION   *)
(* http://localhost:3333/explore *)
(* prometheus --config.file=prometheus.yml
  dune exec bin/runtimevent.exe _build/default/test/main.exe
  /Users/tarides/Desktop/Tarides-Ocaml/prometheus_eventring/grafana-9.1.7/bin/grafana-server -homepath /Users/tarides/Desktop/Tarides-Ocaml/prometheus_eventring/grafana-9.1.7 *)