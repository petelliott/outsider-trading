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
        prompt_ret "GAME OVER"; quit ())
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



let negative_rumor_event (g, w) =
  let bound f = (max 0.0 (min 1.0 f)) in
  let stock = Prob.choice g.stocks in
  Printf.printf "[TODO: negative rumor about %s]\n"
    stock.symbol;
  let resolution (g, w) =
    Printf.printf "[TODO: negative rumor about %s is true]\n" stock.symbol;
    (multiply_stock_price g stock.symbol (bound (Prob.gauss_rand 0.80 0.1)),
     w)
  in (g, (schedule_event resolution
            (g.day + (max 1 (Prob.rand_round
                               (Prob.gauss_rand 3.5 1.0)))) w))

let positive_rumor_event (g, w) =
  let bound f = (max 1.0 f) in
  let stock = Prob.choice g.stocks in
  Printf.printf "[TODO: positive rumor about %s]\n"
    stock.symbol;
  let resolution (g, w) =
    Printf.printf "[TODO: positive rumor about %s is true]\n" stock.symbol;
    (multiply_stock_price g stock.symbol (bound (Prob.gauss_rand 1.2 0.1)),
     w)
  in (g, (schedule_event resolution
            (g.day + (max 1 (Prob.rand_round
                               (Prob.gauss_rand 3.5 1.0)))) w))

let default_script day =
  apply_events
    [ (schedule_event check_margin (1+day));
      (schedule_event ipo 3);
      (schedule_event (set_maxmargin 10000) 5);
      (schedule_event ipo 6);
      (add_random_event negative_rumor_event 0.2);
      (add_random_event positive_rumor_event 0.2) ]
    initial_world
