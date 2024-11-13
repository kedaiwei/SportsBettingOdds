open Printf
open ANSITerminal

exception
  WrongSize of {
    expected_size : int;
    attempted_size : int;
  }

exception Notfound

let emphasize str =
  ANSITerminal.print_string [ on_white; black; ANSITerminal.Bold ] str

let normal_txt str = ANSITerminal.print_string [ on_white; black ] str

(*Helper function that prints "Odds on Team 1 Winning: [odd]."*)
let odd1_printer (odd : float) =
  ANSITerminal.print_string [ on_white; black ] "Odds on Team 1 Winning: ";
  ANSITerminal.printf [ on_white; red ] "%f\n" odd

(*Helper function that prints "Odds on a Draw: [odd]."*)
let drawodd_printer (odd : float) =
  ANSITerminal.print_string [ on_white; black ] "Odds on a Draw: ";
  ANSITerminal.printf [ on_white; red ] "%f\n" odd

(*Helper function that prints "Odds on Team 2 Winning: [odd]."*)
let odd2_printer (odd : float) =
  ANSITerminal.print_string [ on_white; black ] "Odds on Team 2 Winning: ";
  ANSITerminal.printf [ on_white; red ] "%f\n\n" odd

let table_odds_printer (fl_lst : float list option) =
  match fl_lst with
  | Some lst ->
      let l = List.length lst in
      if l = 2 then (
        odd1_printer (List.nth lst 0);
        odd2_printer (List.nth lst 1))
      else if l = 3 then (
        odd1_printer (List.nth lst 0);
        drawodd_printer (List.nth lst 1);
        odd2_printer (List.nth lst 2))
      else raise (WrongSize { expected_size = 2; attempted_size = l })
  | None ->
      ANSITerminal.print_string [ on_white; black ]
        "Odds are not provided for this website\n\n"

(*Helper function to pretty print Team 1.*)
let team1_printer team =
  ANSITerminal.printf
    [ on_red; white; ANSITerminal.Bold ]
    "Team 1: %s   vs   " team

(*Helper function to pretty print Team 2.*)
let team2_printer team =
  ANSITerminal.printf [ on_red; white; ANSITerminal.Bold ] "Team 2: %s " team

let teams_printer (i : int * (string * string)) =
  let a, (t1, t2) = i in
  team1_printer t1;
  team2_printer t2

let site_printer site =
  ANSITerminal.printf [ on_red; white; ANSITerminal.Bold ] "on %s\n" site

let league_printer leag =
  ANSITerminal.printf [ on_red; white; ANSITerminal.Bold ] "in %s " leag

(*Does the same thing as List.iter but with three lists.*)
let rec my_iter4 f lst1 lst2 lst3 lst4 =
  match (lst1, lst2, lst3, lst4) with
  | h1 :: t1, h2 :: t2, h3 :: t3, h4 :: t4 ->
      f h1 h2 h3 h4;
      my_iter4 f t1 t2 t3 t4
  | [], [], [], [] -> ()
  | _ -> raise (Invalid_argument "lists have different lengths")

let print_table (t : Translate.t) =
  my_iter4
    (fun league matchup site odds ->
      teams_printer matchup;
      league_printer league;
      site_printer site;
      table_odds_printer odds)
    (t.league |> List.split |> snd)
    t.game
    (t.site |> List.split |> snd)
    (t.odds |> List.split |> snd)

(************** Helper functions to print out a Calc.t ******************)
let matchup_helper ((t1, t2) : string * string) =
  let t1, t2 = (String.uppercase_ascii t1, String.uppercase_ascii t2) in
  ANSITerminal.printf
    [ on_white; black; ANSITerminal.Bold ]
    "For the matchup %s vs. %s, " t1 t2

let outcome_helper (o : string) =
  if o = "Draw" then
    ANSITerminal.printf
      [ on_white; black; ANSITerminal.Bold ]
      "the match being drawn "
  else
    let o = String.uppercase_ascii o in
    ANSITerminal.printf [ on_white; black; ANSITerminal.Bold ] "%s winning " o

let prob_helper (p : float) =
  ANSITerminal.printf
    [ on_white; black; ANSITerminal.Bold ]
    "has a projected %.2f percent chance of occurring " p

let site_helper (s : string) =
  let s = String.uppercase_ascii s in
  ANSITerminal.printf
    [ on_white; black; ANSITerminal.Bold ]
    "by the odds listed on site %s.\n" s

let bet_helper (b : float option) =
  match b with
  | Some f ->
      ANSITerminal.printf
        [ on_white; black; ANSITerminal.Bold ]
        "If you placed a bet of %.2f on this outcome, " f
  | None -> ()

let payout_helper (p : float option) =
  match p with
  | Some f ->
      ANSITerminal.printf
        [ on_white; black; ANSITerminal.Bold ]
        "you would receive %.2f upon winning.\n" f
  | None -> ()

(************end Helper functions to print out a Calc.t ******************)
let calc_printer (t : Calc.t) =
  matchup_helper t.matchup;
  outcome_helper t.outcome;
  prob_helper t.prob;
  site_helper t.site;
  bet_helper t.bet;
  payout_helper t.payout

(*****)
let print_site (s : string) =
  ANSITerminal.printf [ on_white; black; ANSITerminal.Bold ] "Site: %s\n" s

let print_fl_option (f : float list option) =
  match f with
  | Some lst ->
      ANSITerminal.print_string [ on_white; black; ANSITerminal.Bold ] "(";
      List.iter
        (ANSITerminal.printf [ on_white; black; ANSITerminal.Bold ] "%f ")
        lst;
      ANSITerminal.print_string [ on_white; black; ANSITerminal.Bold ] ")\n"
  | None ->
      ANSITerminal.print_string
        [ on_white; black; ANSITerminal.Bold ]
        "no odds posted for this site\n"

let print_american_matchup (t : Translate.t) (league : string) (l : string list)
    =
  let filtered =
    Calc.league_from_t league (Calc.game_from_t (List.nth l 0, List.nth l 1) t)
  in
  if filtered.game = [] then raise Notfound else ();
  let lst = List.combine filtered.site filtered.odds in
  List.iter
    (fun ((_, str), (_, fl)) ->
      print_site str;
      print_fl_option fl)
    lst

let print_decimal_matchup (t : Translate.t) (league : string) (l : string list)
    =
  let filtered =
    Calc.dec_from_amer (Calc.game_from_t (List.nth l 0, List.nth l 1) t) league
  in
  if filtered.game = [] then raise Notfound else ();
  let lst = List.combine filtered.site filtered.odds in
  List.iter
    (fun ((_, str), (_, fl)) ->
      print_site str;
      print_fl_option fl)
    lst

(*Note: odds are default American, only convert to dec if necessary*)
let print_matchup (t : Translate.t) (league : string) (format : string)
    (l : string list) =
  if List.length l <> 2 then raise Notfound else ();
  match String.lowercase_ascii format with
  | "american" -> print_american_matchup t league l
  | "decimal" -> print_decimal_matchup t league l
  | _ -> raise Notfound

let matchup_comp (_, (t1, t2)) (_, (t3, t4)) =
  (20 * String.compare t1 t3) + (478 * String.compare t2 t4)

(* Prints all matchups in [matchup_lst]*)
let print_matchup_lst_helper matchup_lst =
  List.iter
    (fun (_, (t1, t2)) -> normal_txt (t1 ^ " vs. " ^ t2 ^ "\n\n"))
    matchup_lst

(* Prints all matchups in league [leag] *)
let print_matchup_lst (t : Translate.t) (leag : string) =
  let new_t = Calc.league_from_t leag t in
  print_matchup_lst_helper (List.sort_uniq matchup_comp new_t.game)

let arb_2way_printer (two : (Calc.t * Calc.t) list) =
  if two = [] then
    emphasize "There are no arbitrageable two-way games in the data."
  else
    List.fold_left
      (fun acc (f, s) ->
        calc_printer f;
        calc_printer s)
      () two

let arb_3way_printer (three : (Calc.t * Calc.t * Calc.t) list) =
  if three = [] then
    emphasize "There are no arbitrageable three-way games in the data."
  else
    List.fold_left
      (fun acc (f, s, t) ->
        calc_printer f;
        calc_printer s;
        calc_printer t)
      () three

let arb_printer (t : (Calc.t * Calc.t) list * (Calc.t * Calc.t * Calc.t) list) =
  let two, three = (fst t, snd t) in
  arb_2way_printer two;
  emphasize "\n";
  arb_3way_printer three;
  emphasize "\n"
