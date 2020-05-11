(* future platforms (like bucklescript) must follow this interface *)
val out : string -> unit
val inp : unit -> string
val save_game : Game.game -> unit
val saved_game_exists : unit -> bool
val load_game : Game.game -> Game.game
