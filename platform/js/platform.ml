open Js_of_ocaml

let console_log str =
  ignore (Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log")
            [|Js.Unsafe.inject (Js.string str)|]);
  ()

let prompt str =
  Js.to_string (Js.Unsafe.fun_call (Js.Unsafe.js_expr "prompt")
                  [|Js.Unsafe.inject (Js.string str);
                    Js.Unsafe.inject (Js.string "")|])

let out str =
  console_log str

let inp () =
  prompt "readln"

let save_game g = ()
let saved_game_exists () = false
let load_game dflt = dflt
