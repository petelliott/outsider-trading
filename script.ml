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
        print_endline "GAME OVER"; exit 0)
  else (print_endline "you have made margin call";
        (g, (schedule_event check_margin (g.day + 1) w)))


let default_script () =
  schedule_events
    [ (schedule_event check_margin 1) ]
    initial_world
