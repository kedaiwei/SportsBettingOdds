open Hashtbl

type t = {
  matchup : string * string;
  outcome : string;
  site : string;
  prob : float;
  bet : float option;
  payout : float option;
}

(* Contains odds for a given site *)
type odds = float list

(* Contains sites mapped to odds for a given game *)
type sites = (string, odds) Hashtbl.t

(* Contains all games mapped to sites in a given league *)
type games = (string * string, sites) Hashtbl.t

(* Contains leagues mapped to hashtable containing data on games *)
type league_dic = (string, games) Hashtbl.t

exception NoOdds of string

(* Lower case comparison for strings. *)
let lowercase_comp s1 s2 =
  String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)

(* Helper compare function for matchups which returns true if two matchups are
   the same and false otherwise*)
let matchup_compare (t1, t2) (t3, t4) =
  lowercase_comp t1 t3 = 0 && lowercase_comp t2 t4 = 0

(* Returns all items from a Translate.t in a given column which correspond to
   league [leag]*)
let columns_in_league (t : Translate.t) leag lst =
  List.filter
    (fun (i, _) -> lowercase_comp (List.assoc i t.league) leag = 0)
    lst

(* Returns all items from a Translate.t in a given columns with data for
   [matchup] *)
let columns_in_game (t : Translate.t) matchup lst =
  List.filter (fun (i, _) -> matchup_compare (List.assoc i t.game) matchup) lst

let columns_in_site (t : Translate.t) site lst =
  List.filter (fun (i, _) -> lowercase_comp (List.assoc i t.site) site = 0) lst

(* Returns Translate.t containing table with all rows from leage [leag]*)
let league_from_t (leag : string) (t : Translate.t) : Translate.t =
  {
    league =
      List.filter (fun (_, name) -> lowercase_comp leag name = 0) t.league;
    game = columns_in_league t leag t.game;
    site = columns_in_league t leag t.site;
    odds = columns_in_league t leag t.odds;
  }

(* Returns Translate.t containing table with all rows containing data for
   [matchup]*)
let game_from_t (matchup : string * string) (t : Translate.t) : Translate.t =
  {
    league = columns_in_game t matchup t.league;
    game = List.filter (fun (_, m) -> matchup_compare m matchup) t.game;
    site = columns_in_game t matchup t.site;
    odds = columns_in_game t matchup t.odds;
  }

(* Returns Translate.t containing table with all rows containing data for
   [site]*)
let site_from_t (site : string) (t : Translate.t) : Translate.t =
  {
    league = columns_in_site t site t.league;
    game = columns_in_site t site t.game;
    site = List.filter (fun (_, s) -> lowercase_comp s site = 0) t.site;
    odds = columns_in_site t site t.odds;
  }

(* Returns odds from a given Translate.t Requires: Translate.t is comprised of
   one row. *)
let return_odds (t : Translate.t) =
  match t.odds with
  | hd :: tl -> hd
  | [] -> raise (NoOdds "No entry is found in the table for your query")

(* Initializes site dictionary for a game. Requires: t contains only the
   information for game, site_lst is t.site *)
let rec init_s_dic tbl site_lst (t : Translate.t) =
  match site_lst with
  | (_, site) :: tl -> (
      (* DON'T FORGET TO ADD ONLY ONCE*)
      let site_t = site_from_t site t in
      match return_odds site_t with
      | _, Some odds -> Hashtbl.add tbl site odds
      | _, None -> ())
  | [] -> ()

(* Initializes games dictionary for league. Requires: t contains only the
   information for league, games_lst is t.game *)
let rec init_g_dic tbl games_lst (t : Translate.t) =
  match games_lst with
  | (_, game) :: tl ->
      if not (Hashtbl.mem tbl game) then (
        let game_t = game_from_t game t in
        Hashtbl.add tbl game
          (let s_tbl = Hashtbl.create 9 in
           init_s_dic s_tbl game_t.site game_t;
           s_tbl);
        init_g_dic tbl tl t)
      else ()
  | [] -> ()

(* Initializes dictionary for information in Translate.t *)
let rec init_dic tbl league_lst (t : Translate.t) =
  match league_lst with
  | (_, league) :: tl ->
      if not (Hashtbl.mem tbl league) then (
        let leag_t = league_from_t league t in
        Hashtbl.add tbl league
          (let g_tbl = Hashtbl.create 12 in
           init_g_dic g_tbl leag_t.game leag_t;
           g_tbl);
        init_dic tbl tl t)
      else ()
  | [] -> ()

(* Converts Translate.t type to league_dic type for data processing *)
let convert_to_dic (t : Translate.t) : league_dic =
  let tbl = Hashtbl.create 12 in
  init_dic tbl t.league t;
  tbl

let decimal_payout bet odd = odd *. bet
let decimal_payout_lst odds bet = List.map (decimal_payout bet) odds
let prob_from_decimal odd = 100.0 /. odd
let prob_from_lst odds = List.map prob_from_decimal odds

let decimal_from_american odd =
  if compare odd 0.0 > 0 then (odd /. 100.0) +. 1.0 else 1.0 -. (100.0 /. odd)

let decimal_from_american_lst odds = List.map decimal_from_american odds
let american_payout odd bet = decimal_payout bet (decimal_from_american odd)

let american_payout_lst odds bet =
  decimal_payout_lst (decimal_from_american_lst odds) bet

let prob_from_american odd =
  if compare odd 0.0 > 0 then 100.0 /. (odd +. 100.) *. 100.
  else
    let open Float in
    abs odd /. (abs odd +. 100.) *. 100.

let prob_from_american_lst odds = List.map prob_from_american odds

(* Returns Translate.t containing the row with odds for [matchup] on [site]*)
let row_from_t matchup site (t : Translate.t) : Translate.t =
  site_from_t site (game_from_t matchup t)

(* Make a t given matchup [m], outcome [out], [site], [odd], and [bet] Requires:
   odd is in American form*)
let make m out site bet odd : t =
  {
    matchup = m;
    outcome = out;
    site;
    prob = prob_from_american odd;
    bet;
    payout =
      (match bet with
      | Some b -> Some (american_payout odd b)
      | None -> None);
  }

let bet_anal_helper m site bet (row : Translate.t) =
  match List.nth row.odds 0 with
  | _, Some [ o1; o2; o3 ] ->
      [
        make m (fst m) site bet o1;
        make m "Draw" site bet o2;
        make m (snd m) site bet o3;
      ]
  | _, Some [ o1; o2 ] ->
      [ make m (fst m) site bet o1; make m (snd m) site bet o2 ]
  | _, None | _ -> raise (NoOdds "There are no odds to analyze")
  

let tuple_from_list (sl : string list) :
    (string * string) * string * float option =
  if List.length sl = 3 then
    ((List.nth sl 0, List.nth sl 1), List.nth sl 2, None)
  else if List.length sl = 4 then
    ( (List.nth sl 0, List.nth sl 1),
      List.nth sl 2,
      Some (List.nth sl 3 |> float_of_string) )
  else raise (NoOdds "input length is incorrect")

let bet_anal t leag m site bet =
  let row = t |> league_from_t leag |> game_from_t m |> site_from_t site in
  bet_anal_helper m site bet row

let rec prob_from_odds_helper lst =
  match lst with
  | (i, Some odds) :: t ->
      (i, Some (prob_from_american_lst odds)) :: prob_from_odds_helper t
  | (i, None) :: t -> (i, None) :: prob_from_odds_helper t
  | [] -> []

let prob_from_odds (t : Translate.t) leag : Translate.t =
  let tbl = league_from_t leag t in
  {
    league = tbl.league;
    game = tbl.game;
    site = tbl.site;
    odds = prob_from_odds_helper tbl.odds;
  }

let rec dec_from_amer_helper lst =
  match lst with
  | (i, Some odds) :: t ->
      (i, Some (decimal_from_american_lst odds)) :: dec_from_amer_helper t
  | (i, None) :: t -> (i, None) :: dec_from_amer_helper t
  | [] -> []

let dec_from_amer (t : Translate.t) leag : Translate.t =
  let tbl = league_from_t leag t in
  {
    league = tbl.league;
    game = tbl.game;
    site = tbl.site;
    odds = dec_from_amer_helper tbl.odds;
  }

let closest_matchup (t : Translate.t) = failwith "bruh"
let underdog (t : Translate.t) = failwith "bruh"
let arb (t : Translate.t) bet = failwith "bruh"
