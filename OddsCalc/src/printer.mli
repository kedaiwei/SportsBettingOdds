(**Functions to print lists of floats and strings, used to display data to the
   user.*)

exception
  WrongSize of {
    expected_size : int;
    attempted_size : int;
  }

exception Notfound

val table_odds_printer : float list option -> unit
(**[odds_printer lst] prints three-way and two-way odds contained in [lst].
   Raises exception [WrongSize] if [lst] does not have length 2 or 3.*)

val teams_printer : int * (string * string) -> unit
(**[teams_printer i] prints the teams in the matchup stored in the second
   element of [i]. *)

val print_table : Translate.t -> unit
(**[print_table t] prints all of the odds with associated matchups and betting
   websites in t.*)

val calc_printer : Calc.t -> unit
(**[calc_printer t] prints the results of a single calculation in [t] using
   ANSITerminal.*)

val print_matchup : Translate.t -> string -> string -> string list -> unit
(**[print_matchup t leag form l] prints all matchups played by the two teams
   specified in [l], found in [t], according to the format specified by [form].
   Requires: [s] is either "american" or "decimal"; Raises: Notfound if no such
   matchup can be found in [t], or if [l] does not have exactly two elements.*)

val print_matchup_lst : Translate.t -> string -> unit
(**[print_matchup_lst t leag] prints all of the matchups in [leag] that are
   stored in [t].*)

val arb_printer :
  (Calc.t * Calc.t) list * (Calc.t * Calc.t * Calc.t) list -> unit
(**[arb_printer t] prints the results of all of the calculations deemed to be
   arbitrage-able, stored in [t]. Uses ANSITerminal.*)
