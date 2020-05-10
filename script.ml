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
  then (outln "you did not make your margin call";
        prompt_ret "GAME OVER"; quit ())
  else (outln "you have made margin call";
        (g, (schedule_event check_margin (g.day + 1) w)))


let set_maxmargin n (g, w) =
  out (Printf.sprintf "the bank has changed your margin to $%i\n" n);
  ({ g with maxmargin = n }, w)

let ipo (g, w) =
  let ng = add_stock g in
  let stock = List.hd ng.stocks in
  out (Printf.sprintf "%s corp has entered the market at $%i/share\n"
         stock.symbol stock.price);
  (ng, w)


let resolve_rumor sym mul (g, w) =
  if (Random.float 1.0) < 0.75
  then (out (Printf.sprintf "the rumors about %s turned out to be true!\n" sym);
        (multiply_stock_price g sym mul, w))
  else (g, w)


let negative_rumor_event (g, w) =
  let bound f = (max 0.0 (min 1.0 f)) in
  let stock = Prob.choice g.stocks in
  out (Printf.sprintf "[TODO: negative rumor about %s]\n"
         stock.symbol);
  (g, (schedule_event (resolve_rumor stock.symbol
                         (bound (Prob.gauss_rand 0.80 0.1)))
         (g.day + (max 1 (Prob.rand_round
                            (Prob.gauss_rand 3.5 1.0)))) w))


let positive_rumor_event (g, w) =
  let bound f = (max 1.0 f) in
  let stock = Prob.choice g.stocks in
  out (Printf.sprintf "[TODO: positive rumor about %s]\n"
         stock.symbol);
  (g, (schedule_event (resolve_rumor stock.symbol
                         (bound (Prob.gauss_rand 1.2 0.1)))
         (g.day + (max 1 (Prob.rand_round
                            (Prob.gauss_rand 3.5 1.0)))) w))


let price_events =
  [ (Printf.sprintf "CEO: \"%s stock price too high imo\"\n", 0.8);
    (Printf.sprintf "CEO of %s wins hotdog eating contest\n", 1.2);
    (Printf.sprintf "%s loses $5m bet on the new york mets\n", 0.7);
    (Printf.sprintf "%s finds $3m in office couch cushions\n", 1.3);
    (Printf.sprintf "%s's Q3 earnings are better than expected\n", 1.2);
    (Printf.sprintf "%s's Q1 earnings dissapoint investors\n", 0.8); ]


let stock_price_event (g, w) =
  let stock = Prob.choice g.stocks in
  let (event, mul) = Prob.choice price_events in
  out (event stock.symbol);
  (multiply_stock_price g stock.symbol mul, w)


let default_script day =
  initial_world
  |> schedule_event check_margin (1+day)
  |> schedule_event ipo 3
  |> schedule_event (set_maxmargin 10000) 5
  |> schedule_event ipo 6
  |> add_random_event negative_rumor_event 0.1
  |> add_random_event positive_rumor_event 0.1
  |> add_random_event stock_price_event 0.2
