open Game
open Platform

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
     | _ -> "th") ^ ", 1982"


let num_to_dollars n =
  if n == 0
  then Printf.sprintf "\o033[1m$%i\o033[0m" n
  else if n > 0
  then Printf.sprintf "\o033[1;32m$%i\o033[0m" n
  else Printf.sprintf "\o033[1;31m-$%i\o033[0m" (-n)

let num_to_percent n =
  let perc = (int_of_float (n *. 100.0)) in
  if perc = 0
  then Printf.sprintf "\o033[1m%i%%\o033[0m" perc
  else if perc > 0
  then Printf.sprintf "\o033[1;32m+%i%%\o033[0m" perc
  else Printf.sprintf "\o033[1;31m-%i%%\o033[0m" (-perc)


let out = out
let inp = inp

let newln () =
  out "\n"

let outln str =
  out str;
  newln ()

let print_owned game =
  List.iter
    (fun stock ->
      out
        (Printf.sprintf "%s: %i (%s), "
           stock.symbol stock.owned
           (num_to_dollars (stock.owned * stock.price))))
    game.stocks


let print_portfolio game =
  out (Printf.sprintf "capital: %s, intrest rate: %f\n"
            (num_to_dollars game.capital) game.rate);
  print_owned game;
  out (Printf.sprintf "\nportfolio value: %s\nmargin remaining: %s, \
                         intrest owed: $%f\n"
            (num_to_dollars (portfolio_value game))
            (num_to_dollars (margin_left game))
            (intrest_owed game))

let rec stock_iter2 f a b =
  match (a, b) with
  | ([], []) -> ()
  | (l, []) -> stock_iter2 f l l
  | ([], l) -> stock_iter2 f l l
  | (cara :: cdra, carb :: cdrb) ->
     f cara carb;
     stock_iter2 f cdra cdrb


let print_prices og ng =
  stock_iter2
    (fun o n ->
      out (Printf.sprintf "%s: $%i (%s), "
                n.symbol n.price (num_to_percent (((float_of_int n.price) -.
                                                     float_of_int(o.price)) /.
                                                    float_of_int(o.price)))))
    og.stocks ng.stocks


let prompt_ret str =
  out (Printf.sprintf "%s [RET]: " str);
  ignore (read_line ())

let rec prompt_yn str =
  out (Printf.sprintf "%s [y/n]: " str);
  match (String.uncapitalize_ascii (inp ())) with
  | "y" -> true
  | "n" -> false
  | _   -> prompt_yn str


let clear_screen () =
  out "\o033[2J\o033[H"

let alternate_screen () =
  out "\o033[s\o033[?1047h"

let regular_screen () =
  out "\o033[?1047l\o033[u"

let quit () =
  regular_screen ();
  exit 0

let bold s =
  "\o033[1m" ^ s ^ "\o033[0m"

let dialogue name d =
  (bold name) ^ ": " ^ d
