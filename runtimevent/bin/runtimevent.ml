open Stack
let rec hanoi depart milieu arrivee = function 
| 0 -> ()
| n -> hanoi depart arrivee milieu (n - 1); 
        hanoi milieu depart arrivee (n - 1);;

let pile = Stack.create ()
let list_of_pair = Hashtbl.create 30

let insert_to_hashtable ((k : string), (v : int64 )) (vl : int64 list)= 
  if Hashtbl.mem list_of_pair k = true then 
    Hashtbl.replace list_of_pair k (v :: (Hashtbl.find list_of_pair k))
  else 
    Hashtbl.add list_of_pair k [v]

let compute (phase : string) (ts2 : int64) (_, ts1) = (phase, (Int64.sub ts2 ts1))

(* let rec iter_on_y l = 
  match l with
    |[] -> ()
    |[e] -> 
      Printf.printf "e de iter on y de la list %Ld \n" e 
    |h :: tl -> 
      Printf.printf "h de iter on y de la list%Ld \n" h; iter_on_y tl *)

let runtime_begin_inter ts phase =
  Stack.push (phase, ts) (pile);;

let runtime_begin _ ts phase =
  Stack.push ((Runtime_events.runtime_phase_name phase), (Runtime_events.Timestamp.to_int64 ts)) pile

let runtime_end _ ts phase =
  insert_to_hashtable (compute (Runtime_events.runtime_phase_name phase)
    (Runtime_events.Timestamp.to_int64 ts) (Stack.pop (pile))) []

let () =
  Runtime_events.start ();
  let cursor = Runtime_events.create_cursor None in
  let callbacks = Runtime_events.Callbacks.create ~runtime_begin ~runtime_end ()
  in
    hanoi "A" "B" "C" 3;
    ignore(Runtime_events.read_poll cursor callbacks None);
  Unix.sleep 1

let rec sum_of_list (l:int64 list) count =
  match l with
  |[] ->  count
  |h :: tl -> sum_of_list tl (count + (Int64.to_int h)) 

let _ = Hashtbl.iter (fun phase  ts ->
  let len = List.length ts in
  (* print_string "average of all phase's values :  ";
  let _ = List.fold_left (fun i count -> (Int64.add i count) ) (Int64.of_int 0) y
  in *)
  (Printf.printf "average of all '%s' values : %d \n%!"
    phase ((Int64.to_int (List.fold_left
      (fun i count -> (Int64.add i count) ) (Int64.of_int 0) ts)) / len));
    )list_of_pair
