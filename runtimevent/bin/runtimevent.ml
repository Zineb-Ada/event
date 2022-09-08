open Counter
open Phase
module Event = Runtime_events

let rec hanoi depart milieu arrivee = function 
| 0 -> ()
| n -> hanoi depart arrivee milieu (n - 1); 
        hanoi milieu depart arrivee (n - 1)
let _ =
  Event.start ();
  let cursor = Event.create_cursor None in
  let callbacks = Event.Callbacks.create ~runtime_begin ~runtime_end ~runtime_counter ()
  in
    let _ = String.make 1024 'c' in
    Gc.full_major ();
    hanoi "A" "B" "C" 3;
    ignore(Event.read_poll cursor callbacks None);
    Unix.sleep 1;
    if Array.length Sys.argv = 2  then get_phases ~phase_name:(Sys.argv.(1)) ()
    else if Array.length Sys.argv > 2 then get_phases ~phase_name:(Sys.argv.(1)) ~counter_name_input:(Sys.argv.(2)) ()
    else get_phases ()

(* let () = 
  (* match Array.length Sys.argv with
    |2 -> get_phases ~phase_name:(Sys.argv.(1)) ()
    |(>2) -> *)
  if Array.length Sys.argv = 2  then get_phases ~phase_name:(Sys.argv.(1)) ()
  else if Array.length Sys.argv > 2 then get_phases ~phase_name:(Sys.argv.(1)) ~counter_name_input:(Sys.argv.(2)) ()
  else get_phases () *)
