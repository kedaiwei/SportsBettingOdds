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

let league_calc _ =
  normal_txt
    "\n\
    \ Please enter the kind of calculation you would like to see, filling in \n\
    \ the bracketed items. \n";
  emphasize
    " Home | Matchup List | Calculate [Team1] [Team2] [Site] | Print \
     [American/Decimal] [Team1] [Team2] | Help";
  print_endline "\n"

let rec home _ =
  normal_txt
    "\n Please type the name of the league you would like to see. \n Type '";
  emphasize "Quit";
  normal_txt
    "' to exit at any time. \n\
    \ Your options are posted below. For leagues, type '";
  emphasize "go to [league]";
  normal_txt "'. For \n others, just type the word.  \n";

  ANSITerminal.print_string
    [ on_white; black; ANSITerminal.Bold ]
    " Leagues: NBA | NHL | MLB | Bundesliga | Champions League | Europa League \
     | \n\
    \ Serie A | La Liga | Ligue 1 | MLS | Premier League | NFL | Help ";
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
    | Arbitrage -> "program name"
    | Help -> help_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      home ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      home ()

and help_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n This is the help page. \n";
  normal_txt "\n Please type the kind of help you would like to see \n";
  emphasize
    " Help with Odds | Help with Conversion | \n\
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
    | Arbitrage -> "program name"
    | Home -> home ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      help_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      help_page ()

and nba_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the NBA page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert program"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nba" matchup site float_opt
          |> List.iter Printer.calc_printer;
          nba_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          nba_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "nba" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        nba_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "nba" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        nba_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "nba";
        nba_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      nba_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      nba_page ()

and nhl_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the NHL page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage NHL"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nhl" matchup site float_opt
          |> List.iter Printer.calc_printer;
          nhl_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          nhl_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "nhl" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        nhl_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "nhl" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        nhl_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "nhl";
        nhl_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      nhl_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      nhl_page ()

and mlb_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the MLB page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage mlb"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "mlb" matchup site float_opt
          |> List.iter Printer.calc_printer;
          mlb_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          mlb_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "mlb" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        mlb_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "mlb" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        mlb_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "mlb";
        mlb_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      mlb_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      mlb_page ()

and bundesliga_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Bundesliga page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage bundesliga"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "bundesliga" matchup site float_opt
          |> List.iter Printer.calc_printer;
          bundesliga_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          bundesliga_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "bundesliga" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        bundesliga_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "bundesliga" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        bundesliga_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "bundesliga";
        bundesliga_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      bundesliga_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      bundesliga_page ()

and championsleague_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Champions League page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage championsleague"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "champions league" matchup site
            float_opt
          |> List.iter Printer.calc_printer;
          championsleague_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          nba_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try
           Printer.print_matchup (odds_table ()) "champions league" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        championsleague_page ()
    | Print (Decimal, l) ->
        (try
           Printer.print_matchup (odds_table ()) "champions league" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        championsleague_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "champions league";
        championsleague_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      championsleague_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      championsleague_page ()

and europaleague_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Europa League page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage europaleague"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "europa league" matchup site float_opt
          |> List.iter Printer.calc_printer;
          europaleague_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          europaleague_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "europa league" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        europaleague_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "europa league" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        europaleague_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "europa league";
        europaleague_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      europaleague_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      europaleague_page ()

and serieA_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Serie A page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage serieA"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "serie a" matchup site float_opt
          |> List.iter Printer.calc_printer;
          serieA_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          serieA_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "serie a" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        serieA_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "serie a" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        serieA_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "serie a";
        serieA_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      serieA_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      serieA_page ()

and laliga_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the La Liga page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage La liga"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "la liga" matchup site float_opt
          |> List.iter Printer.calc_printer;
          laliga_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          laliga_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "la liga" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        laliga_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "la liga" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        laliga_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "la liga";
        laliga_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      laliga_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      laliga_page ()

and ligue1_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Ligue 1 page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage NHL"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "ligue 1" matchup site float_opt
          |> List.iter Printer.calc_printer;
          ligue1_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          ligue1_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "ligue 1" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        ligue1_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "ligue 1" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        ligue1_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "ligue 1";
        ligue1_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      ligue1_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      ligue1_page ()

and mls_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the MLS page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage MLS"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "mls" matchup site float_opt
          |> List.iter Printer.calc_printer;
          mls_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          mls_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "mls" "american" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        mls_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "mls" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        mls_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "mls";
        mls_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      mls_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      mls_page ()

and premierleague_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the Premier League page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage premier league"
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "premier league" matchup site float_opt
          |> List.iter Printer.calc_printer;
          premierleague_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          premierleague_page ())
    | Help -> help_page ()
    | Print (American, l) ->
        (try Printer.print_matchup (odds_table ()) "premier league" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        premierleague_page ()
    | Print (Decimal, l) ->
        (try Printer.print_matchup (odds_table ()) "premier league" "decimal" l
         with _ ->
           emphasize "\n Matchup not found. Please enter another command. \n");
        premierleague_page ()
    | MatchupList ->
        Printer.print_matchup_lst (odds_table ()) "premier league";
        premierleague_page ()
    | Quit -> exit 0
    | _ ->
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      premierleague_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
      premierleague_page ()

and nfl_page _ =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; on_red; white ]
    "\n\n Welcome to the NFL page!\n";
  league_calc ();
  try
    match read_line () |> Command.parse with
    | Home -> home ()
    | Arbitrage -> "insert arbitrage NFL "
    | Calculate x -> (
        try
          let matchup, site, float_opt = Calc.tuple_from_list x in
          Calc.bet_anal (odds_table ()) "nfl" matchup site float_opt
          |> List.iter Printer.calc_printer;
          nfl_page ()
        with _ ->
          emphasize
            "\n The matchup was not found. Please enter another command. \n";
          nfl_page ())
    | Help -> help_page ()
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
        emphasize "\n Invalid command. Please try again. \n";
        help_page ()
  with
  | Command.Malformed ->
      emphasize "\n Invalid command. Please try again. \n";
      nfl_page ()
  | Command.Empty ->
      emphasize "\n Please type a command. \n";
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
