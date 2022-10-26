module Counters = struct
  open Prometheus

let namespace = "runtime"
let subsystem = "counter"

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

module Buckets = struct
  let spec = Prometheus.Histogram_spec.of_list [0.; 10.; 50.; 200.; 1000.]
end

module Hist = Prometheus.Histogram(Buckets)
module Phases = struct
  open Prometheus

let namespace = "runtime"
let subsystem = "phases_histogram"

let explicit_gc_set =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "explicit_gc_set"

let explicit_gc_stat =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "explicit_gc_stat"

let explicit_gc_minor =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "explicit_gc_minor"

let explicit_gc_major =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "explicit_gc_major"

let explicit_gc_full_major =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "explicit_gc_full_major"

let explicit_gc_compact =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "explicit_gc_compact"

let major =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major"
  
let major_sweep =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_sweep"
  
let major_mark_roots =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_mark_roots"
  
let major_mark =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_mark"

let minor =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor"
  
let minor_local_roots =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_local_roots"

let minor_finalized =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_finalized"

let explicit_gc_major_slice =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "explicit_gc_major_slice"

let finalise_update_first =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "finalise_update_first"

let finalise_update_last =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "finalise_update_last"

let interrupt_remote =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "interrupt_remote"

let major_ephe_mark =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_ephe_mark"
  
let major_ephe_sweep =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_ephe_sweep"
  
let major_finish_marking =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_finish_marking"
  
let major_gc_cycle_domains =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_gc_cycle_domains"

let major_gc_phase_change =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_gc_phase_change"

let major_gc_stw =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_gc_stw"

let major_mark_opportunistic =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_mark_opportunistic"

let major_slice =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_slice"

let major_finish_cycle =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_finish_cycle"
  
let minor_clear =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_clear"
  
let minor_finalizers_oldify =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_finalizers_oldify"
  
let minor_global_roots =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_global_roots"

let minor_leave_barrier =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_leave_barrier"

let stw_api_barrier =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "stw_api_barrier"

let stw_handler =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "stw_handler"

let stw_leader =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "stw_leader"

let major_finish_sweeping =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "major_finish_sweeping"

let minor_finalizers_admin =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_finalizers_admin"

let minor_remembered_set =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_remembered_set"

let minor_remembered_set_promote =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_remembered_set_promote"

let minor_local_roots_promote =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "minor_local_roots_promote"

let domain_condition_wait =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "domain_condition_wait"

let domain_resize_heap_reservation =
  let help = "" in
  Hist.v ~help ~namespace ~subsystem "domain_resize_heap_reservation" 

end
