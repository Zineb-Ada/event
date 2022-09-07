open Stack
module Event = Runtime_events

type event = {
  ts : int64;
  phase : string;
}

type hash = (string , int64 list) Hashtbl.t
type stack = event Stack.t

type ('a, 'e) result = 
  | Ok of 'a
  | Error of 'e

let rec hanoi depart milieu arrivee = function 
| 0 -> ()
| n -> hanoi depart arrivee milieu (n - 1); 
        hanoi milieu depart arrivee (n - 1)

let dict : hash = Hashtbl.create 30
let pile : stack = Stack.create ()

let compute ev_end =
  match Stack.is_empty pile with
    |false ->
      (let ev_begin = Stack.pop pile in
      let diff = (Int64.sub ev_end.ts ev_begin.ts) in
        match Hashtbl.mem dict ev_begin.phase with
          |false -> 
            if ev_end.phase = ev_begin.phase then Result.ok (Hashtbl.add dict ev_end.phase [diff])
                  else Result.error "the phases are diferente"
          |true -> Result.ok (Hashtbl.replace dict ev_end.phase (diff :: Hashtbl.find dict ev_end.phase)))
    |true -> Result.error (Printf.sprintf "the event '%s' is discarded because there's no begin for it" ev_end.phase)  

let ev_end ts phase = {ts = Event.Timestamp.to_int64 ts; phase = Event.runtime_phase_name phase}

let runtime_begin _ ts phase=
    Stack.push (ev_end ts phase) pile 

let runtime_end _ ts phase =
  match compute (ev_end ts phase) with
    |Ok v -> ()
    |Error e -> Printf.printf "%s \n" e

let () =
  Event.start ();
  let cursor = Event.create_cursor None in
  let callbacks = Event.Callbacks.create ~runtime_begin ~runtime_end ()
  in
      let _ = String.make 1024 'c' in
      Gc.full_major ();
      hanoi "A" "B" "C" 3;
      ignore(Event.read_poll cursor callbacks None);
      Unix.sleep 1

let get_phases ?phase_name ()=
  Hashtbl.iter (fun phase ts ->
    let len = List.length ts in 
    let float_of_list = (Int.to_float ((Int64.to_int (List.fold_left
    (fun i count -> (Int64.add i count)) (Int64.of_int 0) ts)) / len) *. 0.001) in 
      match phase_name with 
      |None -> Printf.printf "The average of all %s values is %f \n" phase float_of_list
      |_ -> if phase_name = Some phase then Printf.printf "The average of %i '%s' values is %f \n" len phase float_of_list
  )dict
let () = if Array.length Sys.argv > 1 then get_phases ~phase_name:(Sys.argv.(1)) () 
  else get_phases ()
