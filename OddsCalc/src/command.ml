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

type object_phrase = string list

type help_category =
  | ArbitrageExpl
  | OddsFormatExpl
  | ConvExpl
  | ProbabilityExpl

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

exception Empty
exception Malformed

(*[match_format str] is a helper function that matches the input str with the
  right [format]. Requires: str is all lowercase. Raises: Malformed if str does
  not match to a valid [format].*)
let match_format str =
  match str with
  | "american" -> American
  | "decimal" -> Decimal
  | _ -> raise Malformed

(*[match_league l] is a helper function that matches the input string list l
  with the right [league]. Requires: str is all lowercase. Raises: Malformed if
  str does not match to a valid [league].*)
let match_league l =
  match l with
  | [ "nba" ] -> NBA
  | [ "nhl" ] -> NHL
  | [ "mlb" ] -> MLB
  | [ "nfl" ] -> NFL
  | [ "bundesliga" ] -> Bundesliga
  | [ "champions"; "league" ] -> ChampionsLeague
  | [ "europa"; "league" ] -> EuropaLeague
  | [ "serie"; "a" ] -> SerieA
  | [ "la"; "liga" ] -> LaLiga
  | [ "ligue"; "1" ] -> Ligue1
  | [ "mls" ] -> MLS
  | [ "premier"; "league" ] -> PremierLeague
  | [ "general" ] -> General
  | _ -> raise Malformed

(*[match_help_type str] is a helper function that matches the input string str
  with the right [help_type]. Requires: str is all lowercase. Raises: Malformed
  if str does not match to a valid [help_type].*)
let match_help_type str =
  match str with
  | "arbitrage" -> ArbitrageExpl
  | "odds" -> OddsFormatExpl
  | "conversion" -> ConvExpl
  | "probability" -> ProbabilityExpl
  | _ -> raise Malformed

(*[match_bet_float f] is a helper function to call in the pattern match of
  parse, attempting to change [f] from a string to a float. Raises: Malformed if
  the string cannot be turned into a float.*)
let match_float f = try float_of_string f with _ -> raise Malformed

let parse str =
  str |> String.lowercase_ascii |> String.split_on_char ' '
  |> List.filter (fun x -> x <> String.empty)
  |> function
  | [] -> raise Empty
  | [ h ] when h = "calculate" -> raise Malformed
  | [ h ] when h = "print" -> raise Malformed
  | [ h ] when h = "bet" -> raise Malformed
  | [ h ] when h = "arbitrage" -> raise Malformed
  | [ h ] when h = "help" -> Help
  | [ h ] when h = "start" -> Start
  | [ h ] when h = "home" -> Home
  | [ h ] when h = "quit" -> Quit
  | [ h ] when h = "riskiest" -> Riskiest
  | [ h ] when h = "closest" -> Closest
  | [ h1; h2 ] when h1 = "go" && h2 = "to" -> raise Malformed
  | [ h1; h2 ] when h1 = "matchup" && h2 = "list" -> MatchupList
  | [ verb1; verb2; league ] when verb1 = "go" && verb2 = "to" ->
      Goto (match_league [ league ])
  | [ verb1; verb2; leaguew1; leaguew2 ] when verb1 = "go" && verb2 = "to" ->
      Goto (match_league [ leaguew1; leaguew2 ])
  | [ verb1; verb2; helptype ] when verb1 = "help" && verb2 = "with" ->
      Helpwith (match_help_type helptype)
  | [ verb; f ] when verb = "arbitrage" -> Arbitrage (match_float f)
  | verb :: t when verb = "quit" && t <> [] -> raise Malformed
  | verb :: t when verb = "home" && t <> [] -> raise Malformed
  | verb :: t when verb = "start" && t <> [] -> raise Malformed
  | verb :: t when verb = "help" && t <> [] -> raise Malformed
  | verb :: t when verb = "riskiest" && t <> [] -> raise Malformed
  | verb :: t when verb = "closest" && t <> [] -> raise Malformed
  | verb :: t when verb = "calculate" -> Calculate t
  | verb :: spec :: t when verb = "print" -> Print (match_format spec, t)
  | verb :: f :: t when verb = "bet" -> Bet (match_float f, t)
  | _ -> raise Malformed
