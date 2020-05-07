(* all the events that occur in the game *)
open Game
open Event
open Print

let rec check_margin (g, w) =
  if (margin_left g) < 0
  then ((prompt_ret "you have borrowed more that availible. \
                     you will lose in 5 days unless you get your \
                     available margin positive");
        (g, (schedule_event margin_loss (g.day + 5) w)))
  else (g, (schedule_event check_margin (g.day + 1) w))
and margin_loss (g, w) =
  if (margin_left g) < 0
  then (print_endline "you did not make your margin call";
        print_endline "GAME OVER"; quit ())
  else (print_endline "you have made margin call";
        (g, (schedule_event check_margin (g.day + 1) w)))


let set_maxmargin n (g, w) =
  Printf.printf "the bank has changed your margin to $%i\n" n;
  ({ g with maxmargin = n }, w)

let ipo (g, w) =
  let ng = add_stock g in
  let stock = List.hd ng.stocks in
  Printf.printf "%s corp has entered the market at $%i/share\n"
    stock.symbol stock.price;
  (ng, w)

let default_script () =
  apply_events
    [ (schedule_event check_margin 1);
      (schedule_event ipo 3);
      (schedule_event (set_maxmargin 10000) 5);
      (schedule_event ipo 6) ]
    initial_world
