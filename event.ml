open Game

type event = Game.game * event_world -> Game.game * event_world
and  event_world = { schedule: (int * event) list;
                     random:   (float * event) list; }

let initial_world = { schedule = []; random = []; }

let rec insert_ordered lst (k, v) =
  match lst with
  | [] -> (k, v) :: []
  | (k2, v2) :: cdr ->
     if k <= k2
     then (k,v) :: ((k2, v2) :: cdr)
     else (k2, v2) :: insert_ordered cdr (k,v)


let schedule_event event day world =
  { world with schedule = insert_ordered world.schedule (day, event) }

let rec apply_events sevts world =
  match sevts with
  | [] -> world
  | car :: cdr -> car (apply_events cdr world)

let add_random_event event prob world =
  { world with random = (prob, event) :: world.random }

let rec do_schedule (game, world) =
  match world.schedule with
  | [] -> (game, world)
  | (day, evt) :: cdr ->
     let nworld = {world with schedule = cdr} in
     if day < game.day
     then do_schedule (game, nworld)
     else if day = game.day
     then do_schedule (evt (game, nworld))
     else (game, world)

let do_random (game, world) =
  let rec inner lst gw =
    match lst with
    | [] -> gw
    | (prob, evt) :: cdr ->
       if (Random.float 1.0) < prob
       then inner cdr (evt gw)
       else inner cdr gw
  in inner world.random (game, world)

let do_event_day gw =
  do_random (do_schedule gw)
