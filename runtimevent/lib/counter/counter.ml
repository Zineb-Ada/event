module Event = Runtime_events

(* type event_counter = {
  tis : int;
  counter_name : string;
} *)

type counter_hash = (string , int list) Hashtbl.t

let counter_dict : counter_hash = Hashtbl.create 30

let runtime_counter _ tis counter_name _ = 
  match Hashtbl.mem counter_dict (Event.runtime_counter_name counter_name) with
    |false -> Hashtbl.add counter_dict (Event.runtime_counter_name counter_name) [Int64.to_int(Event.Timestamp.to_int64 tis)]
    |true -> Hashtbl.replace counter_dict (Event.runtime_counter_name counter_name) 
      (Int64.to_int(Event.Timestamp.to_int64 tis) :: Hashtbl.find counter_dict (Event.runtime_counter_name counter_name))

let get_counter ?counter_name_input ()=
  Hashtbl.iter (fun counter_name tis ->
    let len = List.length tis in 
    match counter_name_input with
    |None -> Printf.printf "number of counters for '%s' is %d \n" counter_name len
    |_ -> if (Some counter_name) = (counter_name_input) 
      then Printf.printf "number of counters for '%s' is %d \n" counter_name len
  )counter_dict
