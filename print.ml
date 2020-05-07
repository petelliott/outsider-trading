open Game

let date day =
  (match (day mod 5) with
   | 0 -> "monday"
   | 1 -> "tuesday"
   | 2 -> "wednesday"
   | 3 -> "thursday"
   | 4 -> "friday"
   | _ -> "err")
  ^ " the " ^ (string_of_int day) ^
    (match (day mod 10) with
     | 1 -> "st"
     | 2 -> "nd"
     | 3 -> "rd"
     | _ -> "th")


let num_to_dollars n =
  if n == 0
  then Printf.sprintf "\o033[1m$%i\o033[0m" n
  else if n > 0
  then Printf.sprintf "\o033[1;32m$%i\o033[0m" n
  else Printf.sprintf "\o033[1;31m-$%i\o033[0m" (-n)

let num_to_percent n =
  let perc = (Float.to_int (n *. 100.0)) in
  if perc = 0
  then Printf.sprintf "\o033[1m%i%%\o033[0m" perc
  else if perc > 0
  then Printf.sprintf "\o033[1;32m+%i%%\o033[0m" perc
  else Printf.sprintf "\o033[1;31m-%i%%\o033[0m" (-perc)

let print_owned game =
  List.iter
    (fun stock ->
      Printf.printf "%s: %i (%s), "
        stock.symbol stock.owned
        (num_to_dollars (stock.owned * stock.price)))
    game.stocks


let print_portfolio game =
  Printf.printf "capital: %s, intrest rate: %f\n"
    (num_to_dollars game.capital) game.rate;
  print_owned game;
  Printf.printf "\nportfolio value: %s\nmargin remaining: %s, intrest owed: $%f\n"
    (num_to_dollars (portfolio_value game))
    (num_to_dollars (margin_left game))
    (intrest_owed game)


let print_prices og ng =
  List.iter2
    (fun o n ->
      Printf.printf "%s: $%i (%s), "
        n.symbol n.price (num_to_percent (((Float.of_int n.price) -.
                                             Float.of_int(o.price)) /.
                                            Float.of_int(o.price))))
    og.stocks ng.stocks


let prompt_ret str =
  Printf.printf "%s [RET]: " str;
  ignore (read_line ())

let newln () =
  print_endline ""
