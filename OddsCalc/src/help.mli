(**Functions to print out explanations for the user, to be called in the REPL.*)

type t = {
  arbitrage : (string * string) list;
  odds : (string * string) list;
  conversion : (string * string) list;
  probability : (string * string) list;
}
(**The type [t] is the representation of the data contained in
   src/explanation.json. Each field of the record type contains an association
   list from string to string, encoding individual segments of the explanation
   so that they can be separated later for clarity.*)

val arbitrage_ex : unit -> unit
(**[arbitrage ()] uses ANSITerminal to print an explanation of how arbitrage
   works for the user.*)

val odds_ex : unit -> unit
(**[odds ()] uses ANSITerminal to print an explanation of what the different
   odds formats mean, and how they should be interpreted. *)

val conversion_ex : unit -> unit
(**[conversion ()] uses ANSITerminal to print an explanation of how odds are
   converted from one format to another.*)

val implied_prob_ex : unit -> unit
(**[implied_prob ()] uses ANSITerminal to print an explanation of how odds can
   be converted to an implied probability of each outcome.*)
