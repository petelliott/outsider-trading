open Yojson.Basic.Util
open Game
open Event

exception Deserialize_exception

let stock stock =
  `Assoc [
      ("symbol", `String stock.symbol);
      ("price", `Int stock.price);
      ("owned", `Int stock.owned);
      ("derivative", `Float stock.derivative);
      ("volatility", `Float stock.volatility) ]


let game game =
  `Assoc [
      ("capital", `Int game.capital);
      ("stocks", `List (List.map stock game.stocks));
      ("rate", `Float game.rate);
      ("day", `Int game.day);
      ("trend", `Float game.trend);
      ("maxmargin", `Int game.maxmargin) ]


let destock = function
  | `Assoc [
      ("symbol", `String sym);
      ("price", `Int price);
      ("owned", `Int owned);
      ("derivative", `Float derivative);
      ("volatility", `Float volatility) ] ->
     { symbol = sym;
       price = price;
       owned = owned;
       derivative = derivative;
       volatility = volatility }
  | _ -> raise Deserialize_exception


let degame = function
  | `Assoc [
      ("capital", `Int capital);
      ("stocks", `List stocks);
      ("rate", `Float rate);
      ("day", `Int day);
      ("trend", `Float trend);
      ("maxmargin", `Int maxmargin) ] ->
     { capital = capital;
       stocks = List.map destock stocks;
       rate = rate;
       day = day;
       trend = trend;
       maxmargin = maxmargin }
  | _ -> raise Deserialize_exception
