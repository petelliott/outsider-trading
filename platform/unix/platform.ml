open Serialize

let out str =
  print_string str

let inp () =
  read_line ()


let save_game_to_file name g =
  let oc = open_out name in
  Yojson.Basic.pretty_to_channel oc (game g);
  output_string oc "\n";
  close_out oc

let save_game = save_game_to_file Sys.argv.(1)

let load_game_from_file name dflt =
  if Sys.file_exists name
  then
    let ic = open_in name in
    let g  = degame (Yojson.Basic.from_channel ic) in
    close_in ic; g
  else
    dflt

let saved_game_exists () =
  Sys.file_exists Sys.argv.(1)

let load_game dflt =
  load_game_from_file Sys.argv.(1) dflt
