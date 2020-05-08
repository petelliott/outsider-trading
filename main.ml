open Game
open Print

let split_capitalize str =
  List.filter (fun s -> s <> "")
    (String.split_on_char ' '
       (String.uppercase_ascii str))

let blk_margin og ng =
  if (margin_left ng) < 0 && (margin_left ng) < (margin_left og)
  then (print_endline "trade has been blocked for insufficient margin";
        og)
  else ng

let process_cmd game str =
  try
    match (split_capitalize str) with
    | ["BUY"; n; stock]  -> blk_margin game (Game.buy game stock (int_of_string n))
    | ["SELL"; n; stock] -> blk_margin game (Game.sell game stock (int_of_string n))
    | ["EXIT"] -> quit () (* TODO  *)
    | [] -> game
    | _ -> print_endline "unknown command"; game
  with _ -> print_endline "invalid arguments"; game

let hour_to_time h =
    match h with
    | 0 -> "9:00"
    | 1 -> "10:00"
    | 2 -> "11:00"
    | 3 -> "12:00"
    | 4 -> "1:00"
    | 5 -> "2:00"
    | 6 -> "3:00"
    | 7 -> "4:00"
    | _ -> raise (Failure "hour out of bounds")


let rec day_loop g h =
  if h == 8
  then (prompt_ret "markets are closed"; newln(); g)
  else (
    Printf.printf "%s %s> " (hour_to_time h)
      (num_to_dollars (available_to_spend g));
    let ng = Game.step_hour (process_cmd g (read_line ()))
    in
    newln();
    print_prices g ng;
    newln ();
    day_loop ng (h + 1))


let rec hidden_day_loop g h =
  if h == 8
  then g
  else hidden_day_loop (Game.step_hour g) (1 + h)

let do_day g =
  if prompt_yn "trade today?"
  then (print_endline "markets are open!"; day_loop g 0)
  else (newln (); (hidden_day_loop g 0))

let rec game_loop save og (g, w) =
  clear_screen ();
  print_endline (date g.day);
  let ng, nw = Event.do_event_day (g, w) in
  save ng;
  newln ();
  print_portfolio ng;
  newln ();
  print_prices og ng;
  newln ();
  game_loop save ng ((Game.step_day (do_day ng)), nw)


let () =
  try
    alternate_screen ();
    Random.self_init ();
    let g = Serialize.load_game_from_file Sys.argv.(1) (Game.initial_game ()) in
    game_loop (Serialize.save_game_to_file Sys.argv.(1))
      g (g, Script.default_script g.day)
  with
  | End_of_file -> regular_screen ()
  | other -> regular_screen (); raise other
