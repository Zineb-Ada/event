open Counter
module Event = Runtime_events

type event = {
  ts : int64;
  phase : string;
}

type hash = (string , int64 list) Hashtbl.t
type stack = event Stack.t

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

let rec max_int_in_list l max = 
  match l with 
    |[] -> max
    |[e] -> if e > max then e else max
    |h :: t -> if h > max then max_int_in_list t h else max_int_in_list t max ;;

let ev_end ts phase = {ts = Event.Timestamp.to_int64 ts; phase = Event.runtime_phase_name phase}

let runtime_begin _ ts phase=
    Stack.push (ev_end ts phase) pile 

let runtime_end _ ts phase =
  match compute (ev_end ts phase) with
    |Ok _ -> ()
    |Error e -> Printf.printf "%s \n" e

let get_phases ?phase_name ?counter_name_input ()=
  get_counter ?counter_name_input ();
  Hashtbl.iter (fun phase ts ->
    let len = List.length ts in
    let float_of_list = (Int.to_float ((Int64.to_int (List.fold_left
    (fun i count -> (Int64.add i count)) (Int64.of_int 0) ts)) / len) *. 0.001) in
      match phase_name with 
        |None -> (match phase with 
          |"minor" | "major" -> Printf.printf "The average of all '%s' values : %f and the biggest one is %f \n" 
            phase float_of_list (Int64.to_float (max_int_in_list ts (Int64.of_int 0)) *. 0.001)
          |_ -> Printf.printf "The average of all '%s' values : %f \n" 
          phase float_of_list)
        |_ -> if phase_name = Some phase then 
          (match Some phase with 
            |Some "minor" | Some "major" -> Printf.printf "The average of all '%s' values : %f and the biggest one is %f \n" 
              phase float_of_list (Int64.to_float (max_int_in_list ts (Int64.of_int 0)) *. 0.001)
            |_ -> Printf.printf "The average of all '%s' values : %f \n" 
            phase float_of_list)
  )dict
