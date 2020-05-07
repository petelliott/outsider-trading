open Game
open Print

let split_capitalize str =
  List.filter (fun s -> s != "")
    (String.split_on_char ' '
       (String.uppercase_ascii str))

let process_cmd game str =
  match (split_capitalize str) with
  | ["BUY"; n; stock]  -> Game.buy game stock (int_of_string n)
  | ["SELL"; n; stock] -> Game.sell game stock (int_of_string n)
  | ["EXIT"] -> exit 0 (* TODO  *)
  | [] -> game
  | _ -> print_endline "unknown command"; game


let rec day_loop g h =
  if h == 8
  then (prompt_ret "markets are closed"; newln(); g)
  else (
    Printf.printf "hour%i>" h;
    let ng = Game.step_hour (process_cmd g (read_line ()))
    in
    print_prices g ng;
    newln ();
    day_loop ng (h + 1))


let rec game_loop og (g, w) =
  print_endline (date g.day);
  print_portfolio g;
  newln ();
  print_prices og g;
  newln ();
  prompt_ret "markets are open!";
  game_loop g (Event.do_event_day
                 ((Game.step_day (day_loop g 0)), w))


let () =
  Random.self_init ();
  let g = Game.initial_game () in
  game_loop g (g, Event.initial_world)
