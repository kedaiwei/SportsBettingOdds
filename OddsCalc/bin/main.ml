open Yojson
open Oddsmath
open ANSITerminal

let scrape _ =
  ignore
    (let cmd = Sys.command "python3 scrape.py" in
     if cmd = 0 then cmd else Sys.command "python.exe scrape.py")

let odds_table _ =
  let json = Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "out.json") in
  Oddsmath.Translate.from_json json

let emphasize str =
  ANSITerminal.print_string [ on_white; black; ANSITerminal.Bold ] str

let normal_txt str = ANSITerminal.print_string [ on_white; black ] str

let print_playing_leagues () =
  emphasize "\n Leagues:\n";
  let leags =
    (odds_table ()).league |> List.split |> snd |> List.sort_uniq compare
  in
  emphasize "   ";
  List.fold_left
    (fun acc s ->
      emphasize s;
      emphasize " | ";
      if acc mod 5 = 0 then emphasize "\n   " else ();
      acc + 1)
    1 leags
  |> ignore;
  emphasize "\n"

let invalid_error_msg () = emphasize "\n Invalid command. Please try again. \n"

let notfound_error_msg () =
  emphasize "\n The matchup was not found. Please enter another command. \n"

let empty_error_msg () = emphasize "\n Please type a command. \n"

let league_calc _ =
  normal_txt
    "\n\
    \ Please enter the kind of calculation you would like to see, filling in \n\
    \ the bracketed items. \n";
  emphasize
    " Home | Matchup List | Print [American/Decimal] [Team1] [Team2] | \
     Calculate [Team1] [Team2] [Site] \n\
    \ Arbitrage [amount to bet] | Riskiest | Closest | Bet [amount to bet] \
     [Team1] [Team2] [Site] | Help";
  print_endline "\n"

let rec home _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n This is the home page. \n";
  normal_txt
    "\n Please type the name of the league you would like to see. \n Type '";
  emphasize "Quit";
  normal_txt
    "' to exit at any time. \n\
    \ Your options are posted below. For leagues, type '";
  emphasize "go to [league]";
  normal_txt "'. For \n others, just type the word.  \n";

  print_playing_leagues ();

  ANSITerminal.print_string
    [ on_white; black; ANSITerminal.Bold ]
    " Arbitrage [bet amount] | Riskiest | Closest | Help ";
  print_endline "\n";

  try
    match read_line () |> Command.parse with
    | Goto NBA -> nba_page ()
    | Goto NHL -> nhl_page ()
    | Goto MLB -> mlb_page ()
    | Goto Bundesliga -> bundesliga_page ()
    | Goto ChampionsLeague -> championsleague_page ()
    | Goto EuropaLeague -> europaleague_page ()
    | Goto SerieA -> serieA_page ()
    | Goto LaLiga -> laliga_page ()
    | Goto Ligue1 -> ligue1_page ()
    | Goto MLS -> mls_page ()
    | Goto PremierLeague -> premierleague_page ()
    | Goto NFL -> nfl_page ()
    | Help -> help_page ()
    | Arbitrage f -> Calc.arb (odds_table ()) f |> Printer.arb_printer
    | Closest ->
        Calc.closest_matchup (odds_table ()) "general" |> Printer.print_table;
        home ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "general" |> Printer.print_table;
        home ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        home ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      home ()
  | Command.Empty ->
      empty_error_msg ();
      home ()

and help_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n This is the help page. \n";
  normal_txt "\n Please type the kind of help you would like to see \n";
  emphasize
    " Help with Arbitrage | Help with Odds | Help with Conversion | \n\
    \ Help with Probability | Go to [league name] | Home ";
  print_endline "\n";

  try
    match read_line () |> Command.parse with
    | Helpwith ArbitrageExpl ->
        Help.arbitrage_ex ();
        help_page ()
    | Helpwith OddsFormatExpl ->
        Help.odds_ex ();
        help_page ()
    | Helpwith ConvExpl ->
        Help.conversion_ex ();
        help_page ()
    | Helpwith ProbabilityExpl ->
        Help.implied_prob_ex ();
        help_page ()
    | Goto NBA -> nba_page ()
    | Goto NHL -> nhl_page ()
    | Goto MLB -> mlb_page ()
    | Goto Bundesliga -> bundesliga_page ()
    | Goto ChampionsLeague -> championsleague_page ()
    | Goto EuropaLeague -> europaleague_page ()
    | Goto SerieA -> serieA_page ()
    | Goto LaLiga -> laliga_page ()
    | Goto Ligue1 -> ligue1_page ()
    | Goto MLS -> mls_page ()
    | Goto PremierLeague -> premierleague_page ()
    | Home -> home ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      help_page ()
  | Command.Empty ->
      empty_error_msg ();
      help_page ()

and nba_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the NBA page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nba" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          nba_page ()
        with _ ->
          notfound_error_msg ();
          nba_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nba" matchup site None
          |> List.iter Printer.calc_printer;
          nba_page ()
        with _ ->
          notfound_error_msg ();
          nba_page ())
    | Closest ->
        Calc.closest_matchup (odds_table ()) "nba" |> Printer.print_table;
        nba_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "nba" |> Printer.print_table;
        nba_page ()
    | Arbitrage f ->
        Calc.arb ~league:"nba" (odds_table ()) f |> Printer.arb_printer;
        nba_page ()
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "nba" "american" l
         with _ -> notfound_error_msg ());
        nba_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "nba" "decimal" l
         with _ -> notfound_error_msg ());
        nba_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "nba";
        nba_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      nba_page ()
  | Command.Empty ->
      empty_error_msg ();
      nba_page ()

and nhl_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the NHL page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nhl" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          nhl_page ()
        with _ ->
          notfound_error_msg ();
          nhl_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nhl" matchup site None
          |> List.iter Printer.calc_printer;
          nhl_page ()
        with _ ->
          notfound_error_msg ();
          nhl_page ())
    | Closest ->
        Calc.closest_matchup (odds_table ()) "nhl" |> Printer.print_table;
        nhl_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "nhl" |> Printer.print_table;
        nhl_page ()
    | Arbitrage f ->
        Calc.arb ~league:"nhl" (odds_table ()) f |> Printer.arb_printer;
        nhl_page ()
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "nhl" "american" l
         with _ -> notfound_error_msg ());
        nhl_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "nhl" "decimal" l
         with _ -> notfound_error_msg ());
        nhl_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "nhl";
        nhl_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      nhl_page ()
  | Command.Empty ->
      empty_error_msg ();
      nhl_page ()

and mlb_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the MLB page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "mlb" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          mlb_page ()
        with _ ->
          notfound_error_msg ();
          mlb_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "mlb" matchup site None
          |> List.iter Printer.calc_printer;
          mlb_page ()
        with _ ->
          notfound_error_msg ();
          mlb_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "mlb" |> Printer.print_table;
        mlb_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "mlb" |> Printer.print_table;
        mlb_page ()
    | Arbitrage f ->
        Calc.arb ~league:"mlb" (odds_table ()) f |> Printer.arb_printer;
        mlb_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "mlb" "american" l
         with _ -> notfound_error_msg ());
        mlb_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "mlb" "decimal" l
         with _ -> notfound_error_msg ());
        mlb_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "mlb";
        mlb_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      mlb_page ()
  | Command.Empty ->
      empty_error_msg ();
      mlb_page ()

and bundesliga_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Bundesliga page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "bundesliga" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          bundesliga_page ()
        with _ ->
          notfound_error_msg ();
          bundesliga_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "bundesliga" matchup site None
          |> List.iter Printer.calc_printer;
          bundesliga_page ()
        with _ ->
          notfound_error_msg ();
          bundesliga_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "bundesliga" |> Printer.print_table;
        bundesliga_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "bundesliga" |> Printer.print_table;
        bundesliga_page ()
    | Arbitrage f ->
        Calc.arb ~league:"bundesliga" (odds_table ()) f |> Printer.arb_printer;
        bundesliga_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "bundesliga" "american" l
         with _ -> notfound_error_msg ());
        bundesliga_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "bundesliga" "decimal" l
         with _ -> notfound_error_msg ());
        bundesliga_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "bundesliga";
        bundesliga_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      bundesliga_page ()
  | Command.Empty ->
      empty_error_msg ();
      bundesliga_page ()

and championsleague_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Champions League page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "champions league" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          championsleague_page ()
        with _ ->
          notfound_error_msg ();
          championsleague_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "champions league" matchup site None
          |> List.iter Printer.calc_printer;
          championsleague_page ()
        with _ ->
          notfound_error_msg ();
          nba_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "champions league"
        |> Printer.print_table;
        championsleague_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "champions league" |> Printer.print_table;
        championsleague_page ()
    | Arbitrage f ->
        Calc.arb ~league:"champions league" (odds_table ()) f
        |> Printer.arb_printer;
        championsleague_page ()
    | Print (American, l) ->
        (try
           Printer.print_matchup (odds_table ()) "champions league" "american" l
         with _ -> notfound_error_msg ());
        championsleague_page ()
    | Print (Decimal, l) ->
        (try
           Printer.print_matchup (odds_table ()) "champions league" "decimal" l
         with _ -> notfound_error_msg ());
        championsleague_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "champions league";
        championsleague_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      championsleague_page ()
  | Command.Empty ->
      empty_error_msg ();
      championsleague_page ()

and europaleague_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Europa League page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "europa league" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          europaleague_page ()
        with _ ->
          notfound_error_msg ();
          europaleague_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "europa league" matchup site None
          |> List.iter Printer.calc_printer;
          europaleague_page ()
        with _ ->
          notfound_error_msg ();
          europaleague_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "europa league"
        |> Printer.print_table;
        europaleague_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "europa league" |> Printer.print_table;
        europaleague_page ()
    | Arbitrage f ->
        Calc.arb ~league:"europa league" (odds_table ()) f
        |> Printer.arb_printer;
        europaleague_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "europa league" "american" l
         with _ -> notfound_error_msg ());
        europaleague_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "europa league" "decimal" l
         with _ -> notfound_error_msg ());
        europaleague_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "europa league";
        europaleague_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      europaleague_page ()
  | Command.Empty ->
      empty_error_msg ();
      europaleague_page ()

and serieA_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Serie A page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "serie a" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          serieA_page ()
        with _ ->
          notfound_error_msg ();
          serieA_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "serie a" matchup site None
          |> List.iter Printer.calc_printer;
          serieA_page ()
        with _ ->
          notfound_error_msg ();
          serieA_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "serie a" |> Printer.print_table;
        serieA_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "serie a" |> Printer.print_table;
        serieA_page ()
    | Arbitrage f ->
        Calc.arb ~league:"serie a" (odds_table ()) f |> Printer.arb_printer;
        serieA_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "serie a" "american" l
         with _ -> notfound_error_msg ());
        serieA_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "serie a" "decimal" l
         with _ -> notfound_error_msg ());
        serieA_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "serie a";
        serieA_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      serieA_page ()
  | Command.Empty ->
      empty_error_msg ();
      serieA_page ()

and laliga_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the La Liga page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "la liga" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          laliga_page ()
        with _ ->
          notfound_error_msg ();
          laliga_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "la liga" matchup site None
          |> List.iter Printer.calc_printer;
          laliga_page ()
        with _ ->
          notfound_error_msg ();
          laliga_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "la liga" |> Printer.print_table;
        laliga_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "la liga" |> Printer.print_table;
        laliga_page ()
    | Arbitrage f ->
        Calc.arb ~league:"la liga" (odds_table ()) f |> Printer.arb_printer;
        laliga_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "la liga" "american" l
         with _ -> notfound_error_msg ());
        laliga_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "la liga" "decimal" l
         with _ -> notfound_error_msg ());
        laliga_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "la liga";
        laliga_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      laliga_page ()
  | Command.Empty ->
      empty_error_msg ();
      laliga_page ()

and ligue1_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Ligue 1 page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "ligue 1" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          ligue1_page ()
        with _ ->
          notfound_error_msg ();
          ligue1_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "ligue 1" matchup site None
          |> List.iter Printer.calc_printer;
          ligue1_page ()
        with _ ->
          notfound_error_msg ();
          ligue1_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "ligue 1" |> Printer.print_table;
        ligue1_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "ligue 1" |> Printer.print_table;
        ligue1_page ()
    | Arbitrage f ->
        Calc.arb ~league:"ligue 1" (odds_table ()) f |> Printer.arb_printer;
        ligue1_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "ligue 1" "american" l
         with _ -> notfound_error_msg ());
        ligue1_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "ligue 1" "decimal" l
         with _ -> notfound_error_msg ());
        ligue1_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "ligue 1";
        ligue1_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      ligue1_page ()
  | Command.Empty ->
      empty_error_msg ();
      ligue1_page ()

and mls_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the MLS page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "mls" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          mls_page ()
        with _ ->
          notfound_error_msg ();
          mls_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "mls" matchup site None
          |> List.iter Printer.calc_printer;
          mls_page ()
        with _ ->
          notfound_error_msg ();
          mls_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "mls" |> Printer.print_table;
        mls_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "mls" |> Printer.print_table;
        mls_page ()
    | Arbitrage f ->
        Calc.arb ~league:"mls" (odds_table ()) f |> Printer.arb_printer;
        mls_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "mls" "american" l
         with _ -> notfound_error_msg ());
        mls_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "mls" "decimal" l
         with _ -> notfound_error_msg ());
        mls_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "mls";
        mls_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      mls_page ()
  | Command.Empty ->
      empty_error_msg ();
      mls_page ()

and premierleague_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Premier League page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "premier league" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          premierleague_page ()
        with _ ->
          notfound_error_msg ();
          premierleague_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "premier league" matchup site None
          |> List.iter Printer.calc_printer;
          premierleague_page ()
        with _ ->
          notfound_error_msg ();
          premierleague_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "premier league"
        |> Printer.print_table;
        premierleague_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "premier league" |> Printer.print_table;
        premierleague_page ()
    | Arbitrage f ->
        Calc.arb ~league:"premier league" (odds_table ()) f
        |> Printer.arb_printer;
        premierleague_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "premier league" "american" l
         with _ -> notfound_error_msg ());
        premierleague_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "premier league" "decimal" l
         with _ -> notfound_error_msg ());
        premierleague_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "premier league";
        premierleague_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      premierleague_page ()
  | Command.Empty ->
      empty_error_msg ();
      premierleague_page ()

and nfl_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the NFL page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Bet (f, x) -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nfl" matchup site (Some f)
          |> List.iter Printer.calc_printer;
          nfl_page ()
        with _ ->
          notfound_error_msg ();
          nfl_page ())
    | Calculate x -> (
        try
          let matchup, site = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nfl" matchup site None
          |> List.iter Printer.calc_printer;
          nfl_page ()
        with _ ->
          notfound_error_msg ();
          nfl_page ())
    | Help -> help_page ()
    | Closest ->
        Calc.closest_matchup (odds_table ()) "nfl" |> Printer.print_table;
        nfl_page ()
    | Riskiest ->
        Calc.underdog (odds_table ()) "nfl" |> Printer.print_table;
        nfl_page ()
    | Arbitrage f ->
        Calc.arb ~league:"nfl" (odds_table ()) f |> Printer.arb_printer;
        nfl_page ()
    | Print (American, l) ->
        Printer.print_matchup (odds_table ()) "nfl" "american" l;
        nfl_page ()
    | Print (Decimal, l) ->
        Printer.print_matchup (odds_table ()) "nfl" "decimal" l;
        nfl_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "nfl";
        nfl_page ()
    | Quit -> exit 0
    | _ ->
        invalid_error_msg ();
        help_page ()
  with
  | Command.Malformed ->
      invalid_error_msg ();
      nfl_page ()
  | Command.Empty ->
      empty_error_msg ();
      nfl_page ()

(* Main function to be called when executed *)
let _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome!\n";
  normal_txt
    "\n\
    \ Are you ready to see your odds for sports leagues?\n\
    \ Note: We do not support gambling. \n\
    \ Type '";
  emphasize "START";
  normal_txt "' to continue or type '";
  emphasize "Quit";
  normal_txt
    "' at any time to exit:\n\
    \ (typing START will retrieve odds which will take a minute or so)";
  print_endline "\n";

  (match read_line () |> Command.parse with
  | Start ->
      scrape ();
      home ()
  | _ -> exit 0)
  |> ignore
