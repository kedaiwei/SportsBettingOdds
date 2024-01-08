(** Parsing of user inputs for REPL. *)

exception Empty
(**Raised when an empty command is parsed.*)

exception Malformed
(**Raised when a malformed command is parsed.*)

type format =
  | American
  | Decimal

type league =
  | NBA
  | NHL
  | MLB
  | NFL
  | Bundesliga
  | ChampionsLeague
  | EuropaLeague
  | SerieA
  | LaLiga
  | Ligue1
  | MLS
  | PremierLeague
  | General

(*The type [help_category] dictates which explanation will be brought up when
  user calls help. [ArbitrageExpl] is an explanation of what arbitrage is and
  how to calculate it. [OddsFormatExpl] is an explanation of the different
  formats of odds. [ConvExpl] is an explanation of how to convert between
  formats. *)
type help_category =
  | ArbitrageExpl
  | OddsFormatExpl
  | ConvExpl
  | ProbabilityExpl

type object_phrase = string list
(** The type [object_phrase] represents the phrase containing the teams matchup
    the user wants to do an operation on. Each element of the list represents
    the word of a command, where a "word" is any sequence of non-space
    characters. No element of the list should have any space characters.

    - If the user inputs ["calculate percentage MEM LAL PointsBet"], the
      object_phrase will be [\"mem"; "lal"; "pointsbet"\]; ]*)

(**The type [command] represents the user's input that is decomposed into the
   verb, specifier, and object phrase. Inv: for Calculate and Print, any
   object_phrase cannot be empty.*)
type command =
  | Calculate of object_phrase
  | Print of format * object_phrase
  | Arbitrage
  | Goto of league
  | Helpwith of help_category
  | Underdog
  | Closest
  | MatchupList
  | Help
  | Start
  | Home
  | Quit

val parse : string -> command
(** [parse str] parses the user's input into a [command]. The first word (i.e.
    consecutive non-space group of characters) becomes the verb. The second word
    becomes the specifier, which is either an [operation], for performing a
    calculation, or a [format], for printing. For example:

    - [parse "print american MEM LAL"] is
      [Print \[American, \["mem"; "lal"\]\]].
    - [parse "calculate percentage MEM LAL PointsBet"] is
      [Calculate \[Percentage, \["mem"; "lal"; "pointsbet"\]\]].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc).

    Raises: [Empty] if [str] is the empty string or only contains spaces.

    Raises: [Malformed] if the command is malformed. That can take any of the
    following forms: The verb is not "calculate", "print", "home", "arbitrage",
    "go to", "help", or "quit"; The verb is "quit", followed by a non-empty
    tail; The verb is "home", followed by a non-empty tail; The verb is "print",
    followed by a non-format specifier; The verb is "calculate", followed by a
    non-operation specifier; The verb is either "print", "calculate",
    "arbitrage", "go to", or "help" but has an empty tail. *)
