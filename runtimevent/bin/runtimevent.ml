open Runtime_events
module Event = Runtime_events

type event = {
  ts : int64;
  phase : Event.runtime_phase;
}

type hash = (Event.runtime_phase , float) Hashtbl.t

let dict : hash = Hashtbl.create 30

let runtime_counter _ _ts counter count =
  let open Prometheus in
  let module C = Metrics.Counters in
  match counter with
  | EV_C_MINOR_PROMOTED ->
    Counter.inc C.minor_promoted (float_of_int count)
  | EV_C_MINOR_ALLOCATED ->
    Counter.inc C.minor_allocated (float_of_int count) 
  | EV_C_FORCE_MINOR_ALLOC_SMALL ->
    Counter.inc C.force_minor_alloc_small (float_of_int count)
  | EV_C_FORCE_MINOR_MAKE_VECT ->
    Counter.inc C.force_minor_make_vect (float_of_int count)
  | EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE ->
    Counter.inc C.force_minor_set_minor_heap_size (float_of_int count)
  | EV_C_FORCE_MINOR_MEMPROF ->
    Counter.inc C.force_minor_memprof (float_of_int count)
  | EV_C_REQUEST_MAJOR_ALLOC_SHR ->
    Counter.inc C.request_major_alloc_shr (float_of_int count)
  | EV_C_REQUEST_MAJOR_ADJUST_GC_SPEED ->
    Counter.inc C.request_major_adjust_gc_speed (float_of_int count)
  | EV_C_REQUEST_MINOR_REALLOC_REF_TABLE ->
    Counter.inc C.request_minor_realloc_ref_table (float_of_int count)
  | EV_C_REQUEST_MINOR_REALLOC_EPHE_REF_TABLE ->
    Counter.inc C.request_minor_realloc_ephe_ref_table (float_of_int count)
  | EV_C_REQUEST_MINOR_REALLOC_CUSTOM_TABLE ->
    Counter.inc C.request_minor_realloc_custom_table (float_of_int count)

let compute ev_end =
    match Hashtbl.mem dict ev_end.phase with
      |true -> Result.ok (ev_end.phase, ((Int64.to_float ev_end.ts) -. (Hashtbl.find dict ev_end.phase)))
      |false -> Result.error (Printf.printf "the event '%s' is discarded because there's no begin for it"  (Event.runtime_phase_name ev_end.phase))

let ev_end ts phase = {ts; phase}

let runtime_begin _ ts phase =
  Hashtbl.add dict phase (Int64.to_float(Event.Timestamp.to_int64 ts))

let runtime_end _ ts phase =
  let open Prometheus in
  let module P = Metrics.Phases in
  let module H = Metrics.Hist in
    let com = compute (ev_end (Event.Timestamp.to_int64 ts) phase) in
      match com with 
        |Ok (p, t) -> 
          (match p with 
            | EV_EXPLICIT_GC_SET -> H.observe P.explicit_gc_set t
            |	EV_EXPLICIT_GC_STAT -> H.observe P.explicit_gc_stat t
            |	EV_EXPLICIT_GC_MINOR -> H.observe P.explicit_gc_minor t
            |	EV_EXPLICIT_GC_MAJOR -> H.observe P.explicit_gc_major t
            |	EV_EXPLICIT_GC_FULL_MAJOR -> H.observe P.explicit_gc_full_major t
            |	EV_EXPLICIT_GC_COMPACT -> H.observe P.explicit_gc_compact t
            |	EV_MAJOR -> H.observe P.major t
            |	EV_MAJOR_SWEEP -> H.observe P.major_sweep t
            |	EV_MAJOR_MARK_ROOTS -> H.observe P.major_mark_roots t
            |	EV_MAJOR_MARK -> H.observe P.major_mark t
            |	EV_MINOR -> H.observe P.minor t
            |	EV_MINOR_LOCAL_ROOTS -> H.observe P.minor_local_roots t
            |	EV_MINOR_FINALIZED -> H.observe P.minor_finalized t
            |	EV_EXPLICIT_GC_MAJOR_SLICE -> H.observe P.explicit_gc_major_slice t
            |	EV_FINALISE_UPDATE_FIRST -> H.observe P.finalise_update_first t
            |	EV_FINALISE_UPDATE_LAST -> H.observe P.finalise_update_last t
            |	EV_INTERRUPT_REMOTE -> H.observe P.interrupt_remote t
            |	EV_MAJOR_EPHE_MARK -> H.observe P.major_ephe_mark t
            |	EV_MAJOR_EPHE_SWEEP -> H.observe P.major_ephe_sweep t
            |	EV_MAJOR_FINISH_MARKING -> H.observe P.major_finish_marking t
            |	EV_MAJOR_GC_CYCLE_DOMAINS -> H.observe P.major_gc_cycle_domains t
            |	EV_MAJOR_GC_PHASE_CHANGE -> H.observe P.major_gc_phase_change t
            |	EV_MAJOR_GC_STW -> H.observe P.major_gc_stw t
            |	EV_MAJOR_MARK_OPPORTUNISTIC -> H.observe P.major_mark_opportunistic t
            |	EV_MAJOR_SLICE -> H.observe P.major_slice t
            |	EV_MAJOR_FINISH_CYCLE -> H.observe P.major_finish_cycle t
            |	EV_MINOR_CLEAR -> H.observe P.minor_clear t
            |	EV_MINOR_FINALIZERS_OLDIFY -> H.observe P.minor_finalizers_oldify t
            |	EV_MINOR_GLOBAL_ROOTS -> H.observe P.minor_global_roots t
            |	EV_MINOR_LEAVE_BARRIER -> H.observe P.minor_leave_barrier t
            |	EV_STW_API_BARRIER -> H.observe P.stw_api_barrier t
            |	EV_STW_HANDLER -> H.observe P.stw_handler t
            |	EV_STW_LEADER -> H.observe P.stw_leader t
            |	EV_MAJOR_FINISH_SWEEPING -> H.observe P.major_finish_sweeping t
            |	EV_MINOR_FINALIZERS_ADMIN -> H.observe P.minor_finalizers_admin t
            |	EV_MINOR_REMEMBERED_SET -> H.observe P.minor_remembered_set t
            |	EV_MINOR_REMEMBERED_SET_PROMOTE -> H.observe P.minor_remembered_set_promote t
            |	EV_MINOR_LOCAL_ROOTS_PROMOTE -> H.observe P.minor_local_roots_promote t
            |	EV_DOMAIN_CONDITION_WAIT -> H.observe P.domain_condition_wait t
            |	EV_DOMAIN_RESIZE_HEAP_RESERVATION -> H.observe P.domain_resize_heap_reservation t)
          |Error _ -> ()

(* Main loop and tracing function borrowed from
https://github.com/patricoferris/runtime-events-demo *)

let tracing child_alive path_pid =
  let open Lwt.Infix in
  let c = create_cursor path_pid in
  let cbs = Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter () in
  let rec looping_over_tracing _ = 
    match child_alive () with 
      | true -> 
        ignore (read_poll c cbs None);
        Lwt_unix.sleep 0.1 >>= fun () ->
        looping_over_tracing ()
      | false -> Lwt.return () 
  in
  looping_over_tracing ()

let () =
  (* Extract the user supplied program and arguments. *)
  let prog, args = Util.prog_args_from_sys_argv Sys.argv in
  let proc =
    Unix.create_process_env prog args
      [| "OCAML_RUNTIME_EVENTS_START=1" |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  Unix.sleepf 0.1;
  Lwt.async@@(fun() -> tracing (Util.child_alive proc) (Some (".", proc)));
  let config = Prometheus_unix.config 8888 in
  Lwt_main.run @@Lwt.choose (Prometheus_unix.serve config);
  Printf.printf "\n"

let () =
  Prometheus_unix.Logging.init ()
  ~default_level:Logs.Debug
  ~levels:[
    "cohttp.lwt.io", Logs.Info;
  ]
