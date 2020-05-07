
type stock = { symbol:     string;
               price:      int;
               owned:      int;
               derivative: float;
               volatility: float; }

type game  = { capital: int;
               stocks:  stock list;
               rate:    float; (* per day interest rate *)
               day:     int;
               trend:   float; } (* 1 day growth rate *)

exception Stock_not_found

let print_list list pfun =
  Printf.printf "[";
  let rec print_sep l =
    match l with
    | [] -> Printf.printf "]"
    | car :: [] -> pfun car; print_sep []
    | car :: cdr -> pfun car;
                    Printf.printf "; ";
                    print_sep cdr
  in print_sep list

(*
let debug_print game =
  Printf.printf "{capital = %i; stocks = " game.capital;
  print_list game.stocks (Printf.printf "%i");
  Printf.printf "; prices = ";
  print_list game.prices (Printf.printf "%i");
  Printf.printf "; names = ";
  print_list game.names (Printf.printf "\"%s\"");
  Printf.printf "; rate = %f; day = %i }\n" game.rate game.day
 *)


let random_letter () =
  Char.chr ((Random.int (90 - 65)) + 65)

let stock_name () =
  Printf.sprintf "%c%c%c"
    (random_letter ())
    (random_letter ())
    (random_letter ())

let add_stock game =
  { game with stocks = {
      symbol     = stock_name ();
      price      = Prob.rand_round (Prob.gauss_rand 100.0 50.0);
      owned      = 0;
      derivative = (Prob.gauss_rand 1.0 0.03);
      volatility = (Prob.gauss_rand 0.0 0.07);
    } :: game.stocks }

let rec add_n_stocks n game =
  if n = 0
  then game
  else add_n_stocks (n-1) (add_stock game)

let initial_game () =
  (add_n_stocks 3
     { capital = 0;
       stocks  = [];
       rate    = 0.01;
       day     = 0;
       trend   = 1.02 })

let apply_to_stock game sym fn =
  let rec inner lst =
    match lst with
    | [] -> []
    | car :: cdr ->
       if car.symbol = sym
       then (fn car) :: cdr
       else car :: inner cdr
  in {game with stocks = inner game.stocks}

let get_stock game sym =
  List.find (fun a -> sym = a.symbol) game.stocks

let buy game sym n =
  apply_to_stock
    { game with capital = game.capital - (get_stock game sym).price * n }
    sym
    (fun stock -> { stock with owned = stock.owned + n })

let sell game sym n =
  buy game sym (-n)

let portfolio_value game =
  let rec stock_value stocks =
    match stocks with
    | [] -> 0
    | car :: cdr -> (car.owned * car.price) + (stock_value cdr)
  in game.capital + (stock_value game.stocks)

let margin game =
  let rec stock_margin stocks =
    match stocks with
    | [] -> 0
    | car :: cdr ->
       (stock_margin cdr) + (max (-(car.owned * car.price)) 0)
  in (stock_margin game.stocks) +
       (max (-game.capital) 0)

let intrest_owed game =
  game.rate *. (Float.of_int (margin game))


let update_stock_price game stock =
  { stock with price = max 0 (Prob.rand_round
                                ((Float.of_int stock.price)
                                 *. stock.derivative *. game.trend));
               derivative = stock.derivative +. (Prob.gauss_rand 0.0 0.01) }


let update_stock_prices game stocks =
  List.map (update_stock_price game) stocks


let fluctuate_stock_price stock =
  { stock with price = max 0 (Prob.rand_round
                                ((Float.of_int stock.price)
                                 *. (Prob.gauss_rand 1.0 (stock.volatility)))) }


let fluctuate_stock_prices stocks =
  List.map fluctuate_stock_price stocks


let step_day game =
  { game with day = game.day + 1;
              stocks = update_stock_prices game game.stocks;
              capital = game.capital - Prob.rand_round (intrest_owed game) }

let step_hour game =
  {game with stocks = fluctuate_stock_prices game.stocks }
