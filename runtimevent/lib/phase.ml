(* module Event = Runtime_events

type event = {
  ts : int64;
  phase : string;
}

type hash = (string , int64) Hashtbl.t

let dict : hash = Hashtbl.create 30

let compute ev_end =
        match Hashtbl.mem dict ev_end.phase with
          |true -> Result.ok (ev_end.phase, (Int64.sub ev_end.ts (Hashtbl.find dict ev_end.phase)))
          |false -> Result.error (Printf.printf "the event '%s' is discarded because there's no begin for it" ev_end.phase) 

let ev_end ts phase = {ts = Event.Timestamp.to_int64 ts; phase = Event.runtime_phase_name phase}

let runtime_begin _ ts phase=
  Hashtbl.add dict phase ts
  (* match phase with
  | EV_EXPLICIT_GC_SET ->
  Counter.inc C.explicit_gc_set (ts) *)
let runtime_end _ ts phase =
  let module C = Metrics.Counters in
  let ev_hash = compute (ev_end ts phase) in
  match ev_hash with 
    | Ok (pn, t) -> 
      (match pn with 
        | explicit_gc_full_major -> 
        Counter.inc C.explicit_gc_full_major (float_of_int(Int64.to_int t))
        | _ -> ()
      )
    | Error e -> e
  (* match ev_hash with
    | (f, EV_EXPLICIT_GC_SET) result -> 
    |Error e -> Printf.printf "%s \n" e;; *)

(* print_string "coucou";; *)
let rec print_phases dic=
  if Hashtbl.length dic < 1 then () else Hashtbl.iter (fun phase ts -> Printf.printf " %s %Ld" phase ts)dic;;
  (* match dic with 
    | empty -> ()
    | (p, ts) :: t -> List.iter (fun t -> Printf.printf "%s %Ld" p t) ts; print_phases t *)

let () = print_phases dict *)
