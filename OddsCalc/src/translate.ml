open Yojson
open Yojson.Basic.Util

type t = {
  league : (int * string) list;
  game : (int * (string * string)) list;
  site : (int * string) list;
  odds : (int * float list option) list;
}

(* Helper Function which removes the outer layer Assoc from the JSON file*)
let remove_outer_layer j = j |> to_assoc

(*Convert the first element of a pair list into ints, which will create
  indices*)
let convert_indices lst = List.map (fun (a, b) -> (int_of_string a, b)) lst

(*De-json-ify the Yojson.Basic.t string on a string*Yojson.Basic.t list *)
let json_string_to_string lst = List.map (fun (a, b) -> (a, to_string b)) lst

(*Gets the column with title League and turns it into an OCaml association list.
  The resulting type is string * Yojson.Basic.t list*)
let get_league_listList f = f |> List.assoc "League" |> to_assoc

(* Returns a list of all the games *)
let combine_league_func json =
  json |> remove_outer_layer |> get_league_listList |> json_string_to_string
  |> convert_indices

(*Gets the column with title Game and turns it into an OCaml association list.
  The resulting type is string * Yojson.Basic.t list*)
let get_game_list f = f |> List.assoc "Game" |> to_assoc

(*Splits the str of form "Team1 vs. Team2 into a string list of form
  ["Team1";"Team2"]. Returns the empty list if the input string does not follow
  the form."*)
let split_team_name str =
  String.split_on_char ' ' str |> function
  | [ el1; el2; el3 ] -> (el1, el3)
  | _ -> ("team not found", "team not found")

(* Returns a list of all the games *)
let combine_game_func json =
  json |> remove_outer_layer |> get_game_list |> json_string_to_string
  |> convert_indices
  |> List.map (fun (a, b) -> (a, split_team_name b))

(*Gets the column with title Site and turns it into an OCaml association list.
  The resulting type is string * Yojson.Basic.t list*)
let get_site_list f = f |> List.assoc "Site" |> to_assoc

(* Returns a list of all the sites*)
let combine_site_funcs json =
  json |> remove_outer_layer |> get_site_list |> json_string_to_string
  |> convert_indices

(*Gets the column with title Odds and turns it into an OCaml association list.
  The resulting type is string * Yojson.Basic.t list*)
let get_odds_list f = f |> List.assoc "Odds" |> to_assoc

(*[safe_to_float j] is Yojson's implementation of to_float, but with a try-catch
  block that replaces empty odds with the float 0*)
let safe_to_float (j : Yojson.Basic.t) =
  try j |> to_string |> float_of_string with _ -> 0.

(*Calls Yojson's to_list to make a Yojson.Basic.t list*)
let convert_odds_to_Yojson_float_lst = List.map (fun (a, b) -> (a, to_list b))

(*Converts the Yojson.Basic.t list from convert_odds_to_Yojson_float_lst to
  float list by using the safe_to_float function*)
let convert_yojsonfl_float_list =
  List.map (fun (a, b) -> (a, List.map safe_to_float b))

(* Converts odds for all events from float list to float option list*)
let make_float_op_list =
  List.map (fun (a, b) ->
      ( a,
        match b with
        | h :: t when h = 0. -> None
        | _ -> Some b ))

(*Composes the two functions above to create the function to format the odds to
  a float list, as the data structure requires.*)
let convert_odds_to_float_op_list syl =
  syl |> convert_odds_to_Yojson_float_lst |> convert_yojsonfl_float_list
  |> make_float_op_list

(* Returns a list of all the Odds*)
let combine_odds_func json =
  json |> remove_outer_layer |> get_odds_list |> convert_odds_to_float_op_list
  |> convert_indices

let from_json j =
  {
    league = combine_league_func j;
    game = combine_game_func j;
    site = combine_site_funcs j;
    odds = combine_odds_func j;
  }
