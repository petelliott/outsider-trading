open Game
open Print


let rec day_loop g h =
  if h == 8
  then (prompt_ret "markets are closed"; newln(); g)
  else (
    Printf.printf "hour%i>" h;
    ignore (read_line ());
    let ng = Game.step_hour g in
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
