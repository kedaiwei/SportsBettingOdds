(** Mathematical functions to perform on odds.*)

type t = {
  matchup : string * string;
  outcome : string;
  site : string;
  prob : float;
  bet : float option;
  payout : float option;
}
(** type t contains the information for a single possible bet. If the user
    provides a value to bet, t contains a bet and payout value *)

exception NoOdds of string
(** Exception raised when an operation is attempted on a matchup and site for
    which odds are not available*)

val league_from_t : string -> Translate.t -> Translate.t
val game_from_t : string * string -> Translate.t -> Translate.t
val site_from_t : string -> Translate.t -> Translate.t

val bet_anal :
  Translate.t -> string -> string * string -> string -> float option -> t list
(**[bet_anal t leag m site bet] is a type t representing the amount that would
   be returned to the bettor if they win on each of the outcomes in the matchup
   contained in [m] respectively on betting site [site], given an initial bet of
   [bet]. Requires: [m] contains two team names in a matchup found in [t], and
   [site] contains a betting site name also found in [t]. *)

val tuple_from_list : string list -> (string * string) * string * float option
(**[tuple_from_list l] checks the length of and decomposes the user input [l]
   from Command.parse, into the triple [matchup, site, bet], to be used in
   [bet_anal].*)

val prob_from_odds : Translate.t -> string -> Translate.t
(** [prob_from_odds leag t] is a Translate.t containing all of the probabilities
    calculated from odds in t in table form. Requires: odds in table are in
    American form. *)

val dec_from_amer : Translate.t -> string -> Translate.t
(**[dec_from_amer t leag] is a Translate.t containing all of the odds in decimal
   form calculated from American odds in t in table form. Requires: Odds in t
   are in American form. *)

val closest_matchup : Translate.t -> string -> Translate.t
(**[closest_matchups leag t] is a Translate.t containing all of the rows of the
   table with the most even odds (usually will be one row). Requires: Odds are
   provided in American form. *)

val underdog : Translate.t -> string -> Translate.t
(**[underdog leag t] is a Translate.t containing all of the rows of the table
   with the most skewed odds (usually will be one row). Requires: Odds in t are
   in American form. *)

val arb : Translate.t -> string -> float option -> (t * t * t option) list
(**[arb t budget] is a type t containing a list of arbitrageable matchups with
   the pertinent data. If budget is Some b then the returned value contains a
   list of the arbitrageable matchups with the associated websites and bets
   placed on each website. If budget is none, the returned value simply contains
   the list of matchups with the associated websites. Requires: Odds in t are in
   American form. *)
