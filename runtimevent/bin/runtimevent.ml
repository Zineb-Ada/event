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
module Event = Runtime_events

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

(* type event = {
  ts : int64;
  phase : Event.runtime_phase;
}

type hash = (Event.runtime_phase , int64 list) Hashtbl.t
type stack = event Stack.t

let dict : hash = Hashtbl.create 30
let pile : stack = Stack.create ()

let compute ev_end =
  (* let diff t p = (Int64.sub (t) (Event.Timestamp.to_int64 (Hashtbl.find dict p))) in
    match Hashtbl.mem dict ev_end.phase with
      |true -> Result.ok (Hashtbl.replace dict ev_end.phase ((diff ev_end.ts ev_end.phase) :: (Hashtbl.find dict ev_end.phase)))
          (* |true -> Result.ok (ev_end.phase, (Int64.sub (Event.Timestamp.to_int64 ev_end.ts) (Event.Timestamp.to_int64 (Hashtbl.find dict ev_end.phase)))) *)
      |false -> Result.error (Printf.printf "the event '%s' is discarded because there's no begin for it"  (Event.runtime_phase_name ev_end.phase))  *)
  match Stack.is_empty pile with
    |false ->
      (let ev_begin = Stack.pop pile in
      let diff = (Int64.sub ev_end.ts ev_begin.ts) in
        match Hashtbl.mem dict ev_begin.phase with
          |false -> 
            if ev_end.phase = ev_begin.phase then Result.ok (Hashtbl.add dict ev_end.phase [diff])
                  else Result.error "the phases are diferente"
          |true -> Result.ok (Hashtbl.replace dict ev_end.phase (diff :: Hashtbl.find dict ev_end.phase)))
    |true -> Result.error (Printf.sprintf "the event '%s' is discarded because there's no begin for it" (Event.runtime_phase_name ev_end.phase))  

let ev_end ts phase = {ts; phase}

let runtime_begin _ ts phase=
  (* Hashtbl.add dict phase ts *)
  Stack.push (ev_end (Event.Timestamp.to_int64 ts) phase) pile
  (* match phase with
  | EV_EXPLICIT_GC_SET ->
  Counter.inc C.explicit_gc_set (ts) *)
let runtime_end _ ts phase =
  match compute (ev_end (Event.Timestamp.to_int64 ts) phase) with 
    |Ok _ ->  Printf.printf "e %s %Ld \n" (Event.runtime_phase_name phase) (Event.Timestamp.to_int64 ts)
    |Error e -> Printf.printf "%s \n" e
  (* with 
    | Ok (pn, []) -> 
      (match pn with 
        | EV_EXPLICIT_GC_FULL_MAJOR -> 
          P.
          (* Histogram_spec.of_linear (P.explicit_gc_full_major ((Int64.to_float t))) *)
        | _ -> ()
      )
    | Error e -> e *)
let event_phases_prom dic = 
  let open Prometheus in
  let module P = Metrics.Phases in
    Hashtbl.iter (fun phase ts -> 
      (* let buckets = Histogram_spec.of_list ts in *)
      match phase with 
        | EV_MAJOR -> 
          (List.iter(fun f ->
            print_string "f:";
            print_float f;
            Gauge.set (P.major) f
          (* Histogram.observe (P.major) f *)
          )ts)
          (* buckets.observe (P.major) *)
        | _ -> ()
    
    )dic *)


type event = {
  ts : int64;
  phase : Event.runtime_phase;
}

type hash = (Event.runtime_phase , int64) Hashtbl.t

let dict : hash = Hashtbl.create 30

let compute ev_end =
  (* let diff t p = (Int64.sub (t) (Event.Timestamp.to_int64 (Hashtbl.find dict p))) in *)
    match Hashtbl.mem dict ev_end.phase with
      (* |true -> Result.ok (Hashtbl.replace dict ev_end.phase ((diff ev_end.ts ev_end.phase) :: (Hashtbl.find dict ev_end.phase))) *)
      |true -> Result.ok (ev_end.phase, (Int64.sub (ev_end.ts) (Hashtbl.find dict ev_end.phase)))
      |false -> Result.error (Printf.printf "the event '%s' is discarded because there's no begin for it"  (Event.runtime_phase_name ev_end.phase)) 
  (* match Stack.is_empty pile with
    |false ->
      (let ev_begin = Stack.pop pile in
      let diff = (Int64.sub ev_end.ts ev_begin.ts) in
        match Hashtbl.mem dict ev_begin.phase with
          |false -> 
            if ev_end.phase = ev_begin.phase then Result.ok (Hashtbl.add dict ev_end.phase [diff])
                  else Result.error "the phases are diferente"
          |true -> Result.ok (Hashtbl.replace dict ev_end.phase (diff :: Hashtbl.find dict ev_end.phase)))
    |true -> Result.error (Printf.sprintf "the event '%s' is discarded because there's no begin for it" (Event.runtime_phase_name ev_end.phase))   *)

let ev_end ts phase = {ts; phase}

let runtime_begin _ ts phase=
  Hashtbl.add dict phase (Event.Timestamp.to_int64 ts)
  (* Stack.push (ev_end (Event.Timestamp.to_int64 ts) phase) pile *)
  (* match phase with
  | EV_EXPLICIT_GC_SET ->
  Counter.inc C.explicit_gc_set (ts) *)
let runtime_end _ ts phase =
  let open Prometheus in
  let module P = Metrics.Phases in
    let com = compute (ev_end (Event.Timestamp.to_int64 ts) phase) in
    (* let rem = Hashtbl.remove dict phase in *)
    match com with 
      | Ok (p, t) -> 
        (match p with 
          | EV_MAJOR -> Gauge.set (P.major) (Int64.to_float t)
          | EV_MINOR -> Gauge.set (P.minor) (Int64.to_float t)
          | _ -> ())
      | error -> ()
      (* let buckets = Histogram_spec.of_list ts in *)
      (* match phase with 
        | EV_MAJOR -> 
          (List.iter(fun f ->
            print_string "f:";
            print_float f;
            Gauge.set (P.major) f
          (* Histogram.observe (P.major) f *)
          )ts)
          (* buckets.observe (P.major) *)
        | _ -> ()
  match compute (ev_end (Event.Timestamp.to_int64 ts) phase) with 
    |Ok _ ->  Printf.printf "e %s %Ld \n" (Event.runtime_phase_name phase) (Event.Timestamp.to_int64 ts)
    |Error e -> Printf.printf "%s \n" e *)
  (* with 
    | Ok (pn, []) -> 
      (match pn with 
        | EV_EXPLICIT_GC_FULL_MAJOR -> 
          P.
          (* Histogram_spec.of_linear (P.explicit_gc_full_major ((Int64.to_float t))) *)
        | _ -> ()
      )
    | Error e -> e *)
(* let event_phases_prom dic = 
  let open Prometheus in
  let module P = Metrics.Phases in
    Hashtbl.iter (fun phase ts -> 
      (* let buckets = Histogram_spec.of_list ts in *)
      match phase with 
        | EV_MAJOR -> 
          (List.iter(fun f ->
            print_string "f:";
            print_float f;
            Gauge.set (P.major) f
          (* Histogram.observe (P.major) f *)
          )ts)
          (* buckets.observe (P.major) *)
        | _ -> ()
    
    )dic *)

 


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






  

  

(*   
  module type NAME = sig
    type t = private string
    val v : string -> t
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end
  
  module Name(N : NAME_SPEC) : NAME = struct
    type t = string
  
    let v name =
      if not (Re.execp N.valid name) then
        failwith (Format.asprintf "Invalid name %S" name);
      name
  
    let compare = String.compare
  
    let pp = Format.pp_print_string
  end
  
  let alphabet = Re.(alt [ rg 'a' 'z'; rg 'A' 'Z' ])
  module LabelName = struct
    (* "^[a-zA-Z_][a-zA-Z0-9_]*$" *)
    let start = Re.alt [ alphabet; Re.char '_' ]
    let rest  = Re.alt [ start; Re.digit ]
    include Name(struct let valid = Re.compile @@ Re.seq [ Re.bos; start; Re.rep rest; Re.eos] end)
  end

  module type BUCKETS = sig
    val spec : Histogram_spec.t
  end
  let bucket_label = label_names.v "le"

  
module Histogram_spec = struct
    type t = float array (* Upper bounds *)
  
    let make at_index_f count =
      let real_at_index i =
        if i >= count then
          infinity
        else
          at_index_f i
      in
      Array.init (count + 1) real_at_index
  
    let of_linear start interval count =
      let at_index i =
        let f = float_of_int i in
        start +. (interval *. f)
      in
      make at_index count
  
    let of_exponential start factor count =
      let at_index i =
        let multiplier = factor ** (float_of_int i) in
        start *. multiplier
      in
      make at_index count
  
    let of_list lst =
      let length = List.length lst in
      make (List.nth lst) length
  
    (* The index at which to record a value [v]. *)
    let index t v =
      let rec aux index =
        if v <= t.(index) then index
        else aux (index + 1)
      in
      aux 0
  end
  
  
  module Histogram (Buckets : BUCKETS) = struct
    module Child = struct
      type t = {
        upper_bounds : Histogram_spec.t;
        counts : float array;
        mutable sum : float;
      }
  
      let create () =
        let count = Array.length Buckets.spec in
        let counts = Array.make count 0. in
        { upper_bounds = Buckets.spec; counts; sum = 0. }
  
      let values t =
        let count = Array.length t.counts in
        let rec fold val_acc acc index =
          if index = count then
            Sample_set.sample ~ext:"_sum" t.sum ::
            Sample_set.sample ~ext:"_count" val_acc ::
            acc
          else
            let val_acc = t.counts.(index) +. val_acc in
            let bucket = (bucket_label, t.upper_bounds.(index)) in
            let acc = Sample_set.sample ~ext:"_bucket" val_acc ~bucket :: acc in
            fold val_acc acc (index + 1)
        in
        fold 0. [] 0
  
      let metric_type = Histogram
  
      let validate_label = function
        | "le" -> failwith "Can't use special label 'le' in histogram"
        | _ -> ()
    end
  
    include Metric(Child)
  
    let observe t v =
      let open Child in
      let index = Histogram_spec.index t.upper_bounds v in
      t.counts.(index) <- t.counts.(index) +. 1.;
      t.sum <- t.sum +. v
  
    let time t gettimeofday fn =
      let start = gettimeofday () in
      Lwt.finalize fn
        (fun () ->
           let finish = gettimeofday () in
           observe t (finish -. start);
           Lwt.return_unit
        )
  end
   *)