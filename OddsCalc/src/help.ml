open ANSITerminal
open Yojson.Basic.Util

type t = {
  arbitrage : (string * string) list;
  odds : (string * string) list;
  conversion : (string * string) list;
  probability : (string * string) list;
}

(*********** Helper functions to read explanation.json *************)
(*Note: we use a json and define the record type along with helper functions to
  decode it so that we can easily expand our explanations page as needed, since
  the size of each of these record fields is unspecified. If we hard-coded
  explanations using longer pipelines, it would be difficult to modify.*)

let data_dir_prefix = "data" ^ Filename.dir_sep

let read_explanation_json =
  Yojson.Basic.from_file (data_dir_prefix ^ "explanation.json")

let de_associate_json (j : Yojson.Basic.t) = j |> to_assoc
let jstring_to_string (js : Yojson.Basic.t) = to_string js

let map_js_to_s (jsl : (string * Yojson.Basic.t) list) =
  List.map (fun (a, b) -> (a, jstring_to_string b)) jsl

let get_arbitrage (lst : (string * Yojson.Basic.t) list) =
  lst |> List.assoc "arbitrage" |> to_assoc |> map_js_to_s

let get_odds (lst : (string * Yojson.Basic.t) list) =
  lst |> List.assoc "odds" |> to_assoc |> map_js_to_s

let get_conversion (lst : (string * Yojson.Basic.t) list) =
  lst |> List.assoc "conversion" |> to_assoc |> map_js_to_s

let get_probability (lst : (string * Yojson.Basic.t) list) =
  lst |> List.assoc "probability" |> to_assoc |> map_js_to_s

let construct_arbitrage =
  read_explanation_json |> de_associate_json |> get_arbitrage

let construct_odds = read_explanation_json |> de_associate_json |> get_odds

let construct_conversion =
  read_explanation_json |> de_associate_json |> get_conversion

let construct_probability =
  read_explanation_json |> de_associate_json |> get_probability

let construct_record =
  {
    arbitrage = construct_arbitrage;
    odds = construct_odds;
    conversion = construct_conversion;
    probability = construct_probability;
  }

let arbitrage_explanation1 =
  construct_record.arbitrage |> List.assoc "explanation1"

let arbitrage_example = construct_record.arbitrage |> List.assoc "example"

let arbitrage_explanation2 =
  construct_record.arbitrage |> List.assoc "explanation1"

let odds_introduction = construct_record.odds |> List.assoc "introduction"

let american_odds_explanation =
  construct_record.odds |> List.assoc "american_explanation"

let decimal_odds_explanation =
  construct_record.odds |> List.assoc "decimal_explanation"

let conversion_explanation1 =
  construct_record.conversion |> List.assoc "explanation1"

let conversion_example1 = construct_record.conversion |> List.assoc "example1"

let conversion_explanation2 =
  construct_record.conversion |> List.assoc "explanation2"

let conversion_example2 = construct_record.conversion |> List.assoc "example2"

let implied_prob_intro =
  construct_record.probability |> List.assoc "introduction"

let implied_prob_dec_expl =
  construct_record.probability |> List.assoc "decimal_explanation"

let implied_prob_dec_example =
  construct_record.probability |> List.assoc "decimal_example"

let implied_prob_am_expl =
  construct_record.probability |> List.assoc "american_explanation"

let implied_prob_am_example =
  construct_record.probability |> List.assoc "american_example"

(*********** Helper functions to read explanation.json *************)

let arbitrage_ex () =
  ANSITerminal.print_string [ on_black; white ] arbitrage_explanation1;
  ANSITerminal.print_string [ on_black; green ] arbitrage_example;
  ANSITerminal.print_string [ on_black; white ] arbitrage_explanation2

let odds_ex () =
  ANSITerminal.print_string [ on_black; white ] odds_introduction;
  ANSITerminal.print_string [ on_black; white ] american_odds_explanation;
  ANSITerminal.print_string [ on_black; white ] decimal_odds_explanation

let conversion_ex () =
  ANSITerminal.print_string [ on_black; white ] conversion_explanation1;
  ANSITerminal.print_string [ on_black; green ] conversion_example1;
  ANSITerminal.print_string [ on_black; white ] conversion_explanation2;
  ANSITerminal.print_string [ on_black; green ] conversion_example2

let implied_prob_ex () =
  ANSITerminal.print_string [ on_black; white ] implied_prob_intro;
  ANSITerminal.print_string [ on_black; white ] implied_prob_am_expl;
  ANSITerminal.print_string [ on_black; green ] implied_prob_am_example;
  ANSITerminal.print_string [ on_black; white ] implied_prob_dec_expl;
  ANSITerminal.print_string [ on_black; green ] implied_prob_dec_example
