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

    - If the user inputs ["calculate MEM LAL PointsBet"], the object_phrase will
      be [\"mem"; "lal"; "pointsbet"\]; ]*)

(**The type [command] represents the user's input that is decomposed into the
   verb, specifier, and object phrase. Inv: for Calculate and Print, any
   object_phrase cannot be empty.*)
type command =
  | Calculate of object_phrase
  | Bet of float * object_phrase
  | Print of format * object_phrase
  | Arbitrage of float
  | Goto of league
  | Helpwith of help_category
  | Riskiest
  | Closest
  | MatchupList
  | Help
  | Start
  | Home
  | Quit

val parse : string -> command
(** [parse str] parses the user's input into a [command]. The first word (i.e.
    consecutive non-space group of characters) becomes the command. For Print
    only, the second word becomes the specifier, which is a [format] for
    printing. The words following Print [format] and following a Calculate
    command become stored as an [object phrase]. The words following the
    commands Goto and Helpwith are interpreted to become their argument. The
    valid arguments for these constructors are given in the REPL loop. Some
    examples of valid parsing:

    - [parse "print american MEM LAL"] is
      [Print \[American, \["mem"; "lal"\]\]].
    - [parse "calculate MEM LAL PointsBet"] is
      [Calculate \["mem"; "lal"; "pointsbet"\]].
    - [parse "gO tO NhL"] is [Goto NHL].
    - [parse "HELP WITH odds"] is [Helpwith OddsFormatExpl]
    - [parse "       home"] is [Home].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc).

    Raises: [Empty] if [str] is the empty string or only contains spaces.

    Raises: [Malformed] if the command is malformed. That can take any of the
    following forms: The verb is not any of the valid constructors in the type
    command; The verb is "quit", "underdog", "closest", "matchup list", "help",
    "start", or "home" followed by a non-empty tail; The verb is "print",
    followed by a non-format specifier; The verb is "bet" or "arbitrage"
    followed by a non-float; The verb is either "print", "calculate", "bet", "go
    to", or "help" but has an empty tail. *)
