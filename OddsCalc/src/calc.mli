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
(** [league_from_t league t] returns a Translate.t with only the rows containg
    data associated with matchups in [league]. *)

val game_from_t : string * string -> Translate.t -> Translate.t
(** [game_from_t matchup t] returns a Translate.t with only the rows containg
    data associated with [matchup]. *)

val site_from_t : string -> Translate.t -> Translate.t
(** [site_from_t site t] returns a Translate.t with only the rows containg data
    associated with matchups posted on betting site [site]. *)

val bet_anal :
  Translate.t -> string -> string * string -> string -> float option -> t list
(**[bet_anal t leag m site bet] is a type t representing the amount that would
   be returned to the bettor if they win on each of the outcomes in the matchup
   contained in [m] respectively on betting site [site], given an initial bet of
   [bet]. Requires: [m] contains two team names in a matchup found in [t], and
   [site] contains a betting site name also found in [t]. *)

val tuple_from_list : string list -> (string * string) * string
(**[tuple_from_list l] checks the length of [l] to be 3 and decomposes the user
   input [l] from Command.parse, into the double [(matchup, site)], to be used
   in [bet_anal]. Raises: NoOdds if length is incorrect.*)

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

val arb :
  Translate.t -> ?league:string -> float -> (t * t) list * (t * t * t) list
(**[arb t league stake] returns a pair of lists of associated bets for arbitrage
   opportunities with total stake [stake]. Note: league is an optional argument.
   If league is passed, arbitrage bets in [league] only will be returned.
   Otherwise, all possible arbitrage opportunities in [t] will be returned. In
   the returned value, the first list in the pair is a list of all possible
   two-way arbitrages, the second list in the pair is a list of all possible
   three way arbitrages. *)
