open Stack
module Event = Runtime_events

type event = {
  ts : int64;
  phase : string;
}
type hash = (string , int64 list) Hashtbl.t
type stack = event Stack.t

let rec hanoi depart milieu arrivee = function 
| 0 -> ()
| n -> hanoi depart arrivee milieu (n - 1); 
        hanoi milieu depart arrivee (n - 1)

let dict : hash = Hashtbl.create 30
let pile : stack = Stack.create ()

let compute ev_end ev_begin=
  let diff = (Int64.sub ev_end.ts ev_begin.ts) in
    match Hashtbl.mem dict ev_begin.phase with
      |false -> if ev_end.phase = ev_begin.phase then Hashtbl.add dict ev_end.phase [diff]
              else ()
      |true -> Hashtbl.replace dict ev_end.phase (diff :: Hashtbl.find dict ev_end.phase)

let ev_end ts phase = {ts= Event.Timestamp.to_int64 ts; phase = Event.runtime_phase_name phase}

let runtime_begin _ ts phase=
    Stack.push (ev_end ts phase) pile

let runtime_end _ ts phase =
  compute (ev_end ts phase) (Stack.pop pile)

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
  print_int len;
  (* print_int len; 
  print_string "len \n";
  print_string "average of all phase's values :  ";
  let _ = List.fold_left (fun i count -> Printf.printf "i : %Ld \n" i; Printf.printf "count: %Ld \n" (Int64.add i count); 
    (Int64.add i count) ) (Int64.of_int 0) ts
  in *)
  (Printf.printf "average of all '%i %s' values : %f ms \n%!"
    len phase (Int.to_float ((Int64.to_int (List.fold_left
      (fun i count -> (Int64.add i count) ) (Int64.of_int 0) ts)) / len) *. 0.001));
    )dict
