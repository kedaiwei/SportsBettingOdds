open Hashtbl
open Array

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

(* Contains sites associated to odds for a given game *)
type sites = (string * odds) list

(* Contains all games mapped to sites in a given league *)
type games = ((string * string) * sites) list

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

(* Initializes site list for a game. Requires: t contains only the information
   for game, site_lst is t.site *)
let rec init_s_dic site_lst (t : Translate.t) : (string * odds) list =
  match site_lst with
  | (_, site) :: tl -> (
      (* Each site is guaranteed to be added only once as each site appears at
         most once per game *)
      let site_t = site_from_t site t in
      match return_odds site_t with
      | _, Some odds -> (site, odds) :: init_s_dic tl t
      | _, None -> init_s_dic tl t)
  | [] -> []

(* Initializes games list for league. Requires: t contains only the information
   for league, games_lst is t.game *)
let rec init_g_dic tbl games_lst (t : Translate.t) =
  match games_lst with
  | (_, game) :: tl ->
      if not (Hashtbl.mem tbl game) then (
        let game_t = game_from_t game t in
        Hashtbl.add tbl game ();
        (game, init_s_dic game_t.site game_t) :: init_g_dic tbl tl t)
      else init_g_dic tbl tl t
  | [] -> []

(* Initializes dictionary for information in Translate.t *)
let rec init_dic tbl league_lst (t : Translate.t) =
  match league_lst with
  | (_, league) :: tl ->
      if not (Hashtbl.mem tbl league) then (
        let leag_t = league_from_t league t in
        Hashtbl.add tbl league
          (let g_tbl = Hashtbl.create 12 in
           init_g_dic g_tbl leag_t.game leag_t);
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

let tuple_from_list (sl : string list) : (string * string) * string =
  if List.length sl = 3 then ((List.nth sl 0, List.nth sl 1), List.nth sl 2)
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

let closest_so_far (fst, s) (i, odds) =
  match odds with
  | Some o when List.length o = 2 ->
      let square x = x *. x in
      let closeness =
        square
          (prob_from_american (List.nth o 0)
          -. prob_from_american (List.nth o 1))
      in
      if closeness < snd !fst then fst := (i, closeness) else ()
  | Some o when List.length o = 3 ->
      let draw_odds = List.nth o 1 in
      if draw_odds < snd !s then s := (i, draw_odds)
  | None -> ()
  | _ -> raise (NoOdds "Odds are of an incorrect length")

let closest_in_t (t : Translate.t) : Translate.t =
  let close_ref = (ref (-1, 10000.), ref (-1, 10000.)) in
  List.iter (closest_so_far close_ref) t.odds;
  let fst_in = close_ref |> fst |> ( ! ) |> fst in
  let snd_in = close_ref |> snd |> ( ! ) |> fst in
  match (fst_in >= 0, snd_in >= 0) with
  | true, true ->
      {
        league =
          (fst_in, List.assoc fst_in t.league)
          :: [ (snd_in, List.assoc snd_in t.league) ];
        game =
          (fst_in, List.assoc fst_in t.game)
          :: [ (snd_in, List.assoc snd_in t.game) ];
        site =
          (fst_in, List.assoc fst_in t.site)
          :: [ (snd_in, List.assoc snd_in t.site) ];
        odds =
          (fst_in, List.assoc fst_in t.odds)
          :: [ (snd_in, List.assoc snd_in t.odds) ];
      }
  | false, true ->
      {
        league = [ (snd_in, List.assoc snd_in t.league) ];
        game = [ (snd_in, List.assoc snd_in t.game) ];
        site = [ (snd_in, List.assoc snd_in t.site) ];
        odds = [ (snd_in, List.assoc snd_in t.odds) ];
      }
  | true, false ->
      {
        league = [ (fst_in, List.assoc fst_in t.league) ];
        game = [ (fst_in, List.assoc fst_in t.game) ];
        site = [ (fst_in, List.assoc fst_in t.site) ];
        odds = [ (fst_in, List.assoc fst_in t.odds) ];
      }
  | false, false -> { league = []; game = []; site = []; odds = [] }

let closest_matchup (t : Translate.t) leag_opt =
  match String.lowercase_ascii leag_opt with
  | "general" -> closest_in_t t
  | s -> t |> league_from_t s |> closest_in_t

(***underdog functions***)
let max_of_float_list (fl : float list) =
  List.fold_left (fun acc x -> if x > acc then x else acc) (List.hd fl) fl

let max_fl_compare (fl1 : float list) (fl2 : float list) =
  match (List.length fl1, List.length fl2) with
  | 2, 2 | 2, 3 | 3, 2 | 3, 3 ->
      if max_of_float_list fl1 > max_of_float_list fl2 then 1
      else if max_of_float_list fl1 < max_of_float_list fl2 then -1
      else 0
  | _ -> raise (NoOdds "Translate.t odd has wrong length")

let floption_compare ((_, l1) : int * float list option)
    ((_, l2) : int * float list option) =
  match (l1, l2) with
  | Some fl1, Some fl2 -> max_fl_compare fl1 fl2
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

let underdog_in_t (t : Translate.t) : Translate.t =
  let sorted = List.sort_uniq floption_compare t.odds |> List.rev in
  let index, _ = List.hd sorted in
  {
    league = [ (index, List.assoc index t.league) ];
    game = [ (index, List.assoc index t.game) ];
    site = [ (index, List.assoc index t.site) ];
    odds = [ (index, List.assoc index t.odds) ];
  }

(***underdog functions***)

let underdog (t : Translate.t) leag_opt =
  match String.lowercase_ascii leag_opt with
  | "general" -> underdog_in_t t
  | s -> t |> league_from_t s |> underdog_in_t

(******** Arbitrage helpers ********)

(*** Twoway arbitrage helpers ***)

(* Appends a set of arbitraged bets to the front of a list for two-way odds. *)
let twoway_lst_cons other_index other_site game site odd o_lst stake arb_lst =
  let other_odd = List.nth o_lst other_index in
  let other_prob = prob_from_american other_odd in
  let prob = prob_from_american odd in
  let bet = stake *. prob in
  let other_bet = stake *. other_prob in
  if prob +. other_prob < 100. then
    ( make game (fst game) site (Some bet) odd,
      make game (snd game) other_site (Some other_bet) other_odd )
    :: arb_lst
  else arb_lst

(* Finds all possible arbitrageable compliments for a given site and odd in
   [site_lst]. Requires: Two-way odds. Process: use iteri to go through the odds
   associated with the given site *)
let rec arb_twoway_helper (game : string * string) (site_lst : sites)
    (site_odds_pair : string * float list) (stake : float)
    (arb_lst : (t * t) list) =
  let arb_lst_ref = ref arb_lst in
  List.iteri
    (fun odd_index odd ->
      match site_lst with
      | (s, o_lst) :: tl -> (
          match odd_index with
          | 0 ->
              arb_lst_ref :=
                twoway_lst_cons 1 s game (fst site_odds_pair) odd o_lst stake
                  !arb_lst_ref
          | 1 ->
              arb_lst_ref :=
                twoway_lst_cons 0 s game (fst site_odds_pair) odd o_lst stake
                  !arb_lst_ref
          | _ -> raise (NoOdds "Wrong index queried"))
      | [] -> ())
    (snd site_odds_pair);
  !arb_lst_ref

(* Finds arbitrage for a given matchup [game]. Folds over the Hashtable game
   folding over each odds list. Requires: matchup has two-way odds. *)
let rec arb_twoway (matchup : string * string) (site_lst : sites)
    (stake : float) (arb_lst : (t * t) list) =
  match site_lst with
  | (site, odds) :: tl ->
      List.fold_left
        (fun acc _ -> arb_twoway_helper matchup site_lst (site, odds) stake acc)
        arb_lst tl
      @ arb_twoway matchup tl stake arb_lst
  | [] -> arb_lst

(*** Threeway arbitrage helpers ***)

(* Appends a set of arbitraged bets to the front of a list for three-way odds if
   the odds can be arbitraged. *)
let threeway_lst_cons game site_1 site_2 site_3 odd_1 odd_2 odd_3 stake arb_lst
    =
  let prob_1 = prob_from_american odd_1 in
  let prob_2 = prob_from_american odd_2 in
  let prob_3 = prob_from_american odd_3 in
  let bet_1 = stake *. prob_1 in
  let bet_2 = stake *. prob_2 in
  let bet_3 = stake *. prob_3 in
  if prob_1 +. prob_2 +. prob_3 < 100. then
    ( make game (fst game) site_1 (Some bet_1) odd_1,
      make game "Draw" site_2 (Some bet_2) odd_2,
      make game (snd game) site_3 (Some bet_3) odd_3 )
    :: arb_lst
  else arb_lst

let split_lst_helper matchup site_odd_pair split_site_lst site split_odds stake
    arb_lst =
  let arb_lst_ref = ref arb_lst in
  List.iteri
    (fun odd_index odd ->
      match split_site_lst with
      | (s, o_lst) :: tl -> (
          match odd_index with
          | 0 ->
              arb_lst_ref :=
                threeway_lst_cons matchup (fst site_odd_pair) site s
                  (snd site_odd_pair) odd (List.nth o_lst 1) stake !arb_lst_ref
          | 1 ->
              arb_lst_ref :=
                threeway_lst_cons matchup (fst site_odd_pair) site s
                  (snd site_odd_pair) odd (List.nth o_lst 0) stake !arb_lst_ref
          | _ -> raise (NoOdds "Wrong index queried"))
      | [] -> ())
    split_odds;
  !arb_lst_ref

let arb_threeway_helper matchup split_site_lst site_odd_pair stake arb_lst =
  match split_site_lst with
  | (site, split_odds) :: tl ->
      List.fold_left
        (fun acc _ ->
          split_lst_helper matchup site_odd_pair split_site_lst site split_odds
            stake acc)
        arb_lst tl
  | [] -> arb_lst

(* Finds arbitrage for a given [matchup]. Folds over the Hashtable game folding
   over each odds list. Requires: matchup has three-way odds. *)
let rec arb_threeway (matchup : string * string) (site_lst : sites)
    (stake : float) (arb_lst : (t * t * t) list) =
  let split_site_lst =
    List.fold_left
      (fun acc (site, odds) -> (site, List.tl odds) :: acc)
      [] site_lst
  in
  match site_lst with
  | (site, odds) :: tl ->
      arb_threeway_helper matchup split_site_lst
        (site, List.hd odds)
        stake arb_lst
  | [] -> arb_lst

(*** Grand scheme arbitrage helpers ***)

(** Determines whether or not matchup has twoway or threeway odds. *)
let arb_matchup (matchup : string * string) (site_lst : sites) (stake : float)
    (arb_lst : (t * t) list * (t * t * t) list) =
  match site_lst with
  | (site, odds) :: tl when List.length odds = 2 ->
      (arb_twoway matchup site_lst stake (fst arb_lst), snd arb_lst)
  | (site, odds) :: tl when List.length odds = 3 ->
      (fst arb_lst, arb_threeway matchup site_lst stake (snd arb_lst))
  | _ -> arb_lst

(* Finds arbitrage for all matchups in [game_lst]. *)
let arb_league (game_lst : games) (stake : float)
    (arb_lst : (t * t) list * (t * t * t) list) =
  List.fold_left
    (fun acc (game, site_lst) -> arb_matchup game site_lst stake acc)
    arb_lst game_lst

(* Converts [t] into a dictionary and performs arbitrage calculation on each
   matchup in the dictionary. If league is passed, arbitrage calculation is only
   performed on a given list. *)
let arb (t : Translate.t) ?(league = "general") (stake : float) =
  let tbl = convert_to_dic t in
  match league with
  | "general" ->
      Hashtbl.fold (fun _ games acc -> arb_league games stake acc) tbl ([], [])
  | s -> arb_league (Hashtbl.find tbl s) stake ([], [])
