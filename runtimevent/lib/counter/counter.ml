(* open Core.Std *)
module Event = Runtime_events

type counter_hash = (string , int list) Hashtbl.t

let counter_dict : counter_hash = Hashtbl.create 30

let runtime_counter _ tis counter_name _ = 
  match Hashtbl.mem counter_dict (Event.runtime_counter_name counter_name) with
    |false -> Hashtbl.add counter_dict (Event.runtime_counter_name counter_name) [Int64.to_int(Event.Timestamp.to_int64 tis)]
    |true -> Hashtbl.replace counter_dict (Event.runtime_counter_name counter_name) 
      (Int64.to_int(Event.Timestamp.to_int64 tis) :: Hashtbl.find counter_dict (Event.runtime_counter_name counter_name))

let get_counter ?counter_name_input ()=
  Hashtbl.iter (fun counter_name tis ->
    let ltis = tis in
    let len = List.length tis in 
    match counter_name_input with
      (* |None -> List.iter (fun c -> Printf.printf "%s %d \n" counter_name c) tis *)
      (* let rec  *)
      (* |None -> match tis with  *)
        (* | [] ->  *)
      | None -> (let _ = Printf.printf "%s " counter_name in 
        (* let tl = [] in *)
        let rec loop l = 
          match l with 
            | [] -> () 
            | [e] -> Printf.printf "%d \n" e
            | h :: t -> if h = List.hd ltis then (Printf.printf "%d;X " h; loop t) else (Printf.printf "%d " h; loop t) in
        loop tis) 
        (* List.iter (fun c -> Printf.printf " %d \n"  c) tis *)
      (* Printf.sprintf "%s %i" counter_name tis *)
      (* List.iter (fun c -> Printf.printf "%s %d \n" counter_name c) tis *)

      (* Printf.printf "number of counters for '%s' is %d \n" counter_name len *)
      |_ -> 
      (* Printf.sprintf "%s %d" counter_name len *)
      if (Some counter_name) = (counter_name_input) 
        then Printf.printf "number of counters for '%s' is %d \n" counter_name len
  )counter_dict

  (* let () = Out_channel.write_all "your_file.txt" ~data:"Your text" *)