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
      | _ -> () 

    (* Main loop and tracing function borrowed from
    https://github.com/patricoferris/runtime-events-demo *)

    let tracing child_alive path_pid =
      let c = create_cursor path_pid in
      let cbs = Callbacks.create ~runtime_counter () in
      while child_alive () do
        ignore (read_poll c cbs None);
        Unix.sleepf 0.1
      done

  let () =
    (* Extract the user supplied program and arguments. *)
    let prog, args = Util.prog_args_from_sys_argv Sys.argv in
    let proc =
      Unix.create_process_env prog args
        [| "OCAML_RUNTIME_EVENTS_START=1" |]
        Unix.stdin Unix.stderr Unix.stdout
    in
    Unix.sleepf 0.1;
    tracing (Util.child_alive proc) (Some (".", proc));
    Lwt_main.run (Prometheus_unix.serve )
    Printf.printf "\n"


let () =
  Prometheus_unix.Logging.init ()
  ~default_level:Logs.Debug
  ~levels:[
    "cohttp.lwt.io", Logs.Info;
  ]