(* open Stack

let rec hanoi depart milieu arrivee = function 
| 0 -> ()
| n -> hanoi depart arrivee milieu (n - 1); 
        hanoi milieu depart arrivee (n - 1);;

let pile = Stack.create ()
let list_of_pair = Hashtbl.create 30

let insert_to_hashtable ((k : string), (v : int64 ))= 
  if Hashtbl.mem list_of_pair k = true then 
    Hashtbl.replace list_of_pair k (v :: (Hashtbl.find list_of_pair k))
  else 
    Hashtbl.add list_of_pair k [v]

(* let compute (phase : string) (ts2 : int64) (_, ts1) = 
  (phase, (Int64.sub ts2 ts1)) *)

let compute (phase2 : string) (ts2 : int64) (phase1, ts1) = 
  if phase1 = phase2 then (phase2, (Int64.sub ts2 ts1))
  else failwith "there's no b or e"

let runtime_begin_inter ts phase =
  Stack.push (phase, ts) (pile);;

let runtime_begin _ ts phase =
  Stack.push ((Runtime_events.runtime_phase_name phase), (Runtime_events.Timestamp.to_int64 ts)) pile

let runtime_end _ ts phase =
  insert_to_hashtable (compute (Runtime_events.runtime_phase_name phase)
    (Runtime_events.Timestamp.to_int64 ts) (Stack.pop (pile)))

let () =
  Runtime_events.start ();
  let cursor = Runtime_events.create_cursor None in
  let callbacks = Runtime_events.Callbacks.create ~runtime_begin ~runtime_end ()
  in
    hanoi "A" "B" "C" 3;
    ignore(Runtime_events.read_poll cursor callbacks None);
  Unix.sleep 1

let _ = Hashtbl.iter (fun phase ts ->
  let len = List.length ts in
  (* print_int len; 
  print_string "len \n";
  print_string "average of all phase's values :  ";
  let _ = List.fold_left (fun i count -> Printf.printf "i : %Ld \n" i; Printf.printf "count: %Ld \n" (Int64.add i count); 
    (Int64.add i count) ) (Int64.of_int 0) ts
  in *)
  (Printf.printf "average of all '%i %s' values : %f ms \n%!"
    len phase (Int.to_float ((Int64.to_int (List.fold_left
      (fun i count -> (Int64.add i count) ) (Int64.of_int 0) ts)) / len) *. 0.001));
    )list_of_pair *)

    open Runtime_events

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

    (* Main loop and tracing function borrowed from
    https://github.com/patricoferris/runtime-events-demo *)

    let tracing child_alive path_pid =
      let open Lwt.Infix in
      let c = create_cursor path_pid in
      let cbs = Callbacks.create ~runtime_counter () in
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