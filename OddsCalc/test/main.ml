(******************************************************************************
  We elected to use OUnit mainly to test the backend systems that power our data
  interpretation and REPL interface. So, in this test file we tested our
  translate file and our Command file. For Translate, we created a number of
  different JSON files. Each of these JSON files have different configurations
  of possible games. For example, we created a JSON file with multiple games,
  one with NONE in their odds, some with different sites, and some with
  different Leagues. We created all the different possible combinations of
  potential JSON's to ensure a thorough testing on all edge cases when
  considering possible translations. For our Command file, we tested all the
  different potential inputs a user can input. This included multiple spaces in
  between the different key words, case insensitivity, and different mispelling
  of words. We made sure to test all the different possible inputs a user can
  put in to ensure that they all work. We wanted to interactively test the main
  file because we wanted to simulate the user experience and ensure that it was
  of the highest quality. By playing through it, we were able to improve how we
  presented our menu (including font, highlights, order of options) in addition
  to making sure all of the functionalities are in place. The Calc function will
  also be tested interactively, because the only times that those functions are
  called are directly followed by a call to print the data. So, we can quickly
  debug something that would be impossible to write the expectation for (since
  we would have to browse a several-thousand line json and find the maximum
  manually, for example). We are confident that our tests comprehensively test
  correctness of our system because our OUnit tests have been written to cover
  all of the possible commands and jsons, and our interactive testing went over
  every single branch in every menu.
  ************************************************************************)

open Yojson.Basic
open Oddsmath
open Oddsmath.Translate
open Oddsmath.Command
open OUnit2

(* Helper Functions to read the different testing files *)
let data_dir_prefix = "data" ^ Filename.dir_sep
let test_trivial = Yojson.Basic.from_file (data_dir_prefix ^ "test_trivial.json")

let test_OddsNone =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_OddsNone.json")

let test_multGames =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_multiple_games.json")

let test_multGames_None =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_multgamesNone.json")

let test_multGames_ALLNone =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_multgamesALLNone.json")

let test_multLeagues =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_multLeagues.json")

let test_noneMiddle =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_noneMiddle.json")

let test_noneLast =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_noneLast.json")

let test_multLeaguesNone =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_multLeaguesNone.json")

let test_multSites =
  Yojson.Basic.from_file (data_dir_prefix ^ "test_multSites.json")

let test_uneven = Yojson.Basic.from_file (data_dir_prefix ^ "test_uneven.json")

(* Testing functions to check translation of JSON's *)
let translate_tests =
  [
    ( "trivial_test" >:: fun _ ->
      assert_equal
        {
          league = [ (1, "La Liga") ];
          game = [ (1, ("Barca", "Madrid")) ];
          site = [ (1, "games.com") ];
          odds = [ (1, Some [ 1.0; 1.0; 1.0 ]) ];
        }
        (from_json test_trivial) );
    ( "oddsNone_test" >:: fun _ ->
      assert_equal
        {
          league = [ (1, "La Liga") ];
          game = [ (1, ("Barca", "Madrid")) ];
          site = [ (1, "games.com") ];
          odds = [ (1, None) ];
        }
        (from_json test_OddsNone) );
    ( "multiple_games_test" >:: fun _ ->
      assert_equal
        {
          league = [ (1, "La Liga"); (2, "La Liga"); (3, "La Liga") ];
          game =
            [
              (1, ("Barca", "Madrid"));
              (2, ("Madrid", "Athletico"));
              (3, ("Barca", "Athletico"));
            ];
          site = [ (1, "games.com"); (2, "games.com"); (3, "games.com") ];
          odds =
            [
              (1, Some [ -182.0; 400.0; 320.0 ]);
              (2, Some [ -165.0; 425.0; 370.0 ]);
              (3, Some [ -170.0; 400.0; 370.0 ]);
            ];
        }
        (from_json test_multGames) );
    ( "multiple_games_None_test" >:: fun _ ->
      assert_equal
        {
          league = [ (1, "La Liga"); (2, "La Liga"); (3, "La Liga") ];
          game =
            [
              (1, ("Barca", "Madrid"));
              (2, ("Madrid", "Athletico"));
              (3, ("Barca", "Athletico"));
            ];
          site = [ (1, "games.com"); (2, "games.com"); (3, "games.com") ];
          odds =
            [
              (1, None);
              (2, Some [ -165.0; 425.0; 370.0 ]);
              (3, Some [ -170.0; 400.0; 370.0 ]);
            ];
        }
        (from_json test_multGames_None) );
    ( "multiple_games_ALLNone_test" >:: fun _ ->
      assert_equal
        {
          league = [ (1, "La Liga"); (2, "La Liga"); (3, "La Liga") ];
          game =
            [
              (1, ("Barca", "Madrid"));
              (2, ("Madrid", "Athletico"));
              (3, ("Barca", "Athletico"));
            ];
          site = [ (1, "games.com"); (2, "games.com"); (3, "games.com") ];
          odds = [ (1, None); (2, None); (3, None) ];
        }
        (from_json test_multGames_ALLNone) );
    ( "multLeagues_test" >:: fun _ ->
      assert_equal
        {
          league =
            [
              (1, "La Liga");
              (2, "La Liga");
              (3, "La Liga");
              (4, "Premier League");
              (5, "Ligue One");
            ];
          game =
            [
              (1, ("Barca", "Madrid"));
              (2, ("Madrid", "Athletico"));
              (3, ("Barca", "Athletico"));
              (4, ("Chelsea", "NewCastle"));
              (5, ("PSG", "Nice"));
            ];
          site =
            [
              (1, "games.com");
              (2, "games.com");
              (3, "games.com");
              (4, "games.com");
              (5, "games.com");
            ];
          odds =
            [
              (1, Some [ -182.0; 400.0; 320.0 ]);
              (2, Some [ -165.0; 425.0; 370.0 ]);
              (3, Some [ -170.0; 400.0; 370.0 ]);
              (4, Some [ -170.0; 400.0; 370.0 ]);
              (5, Some [ -170.0; 400.0; 370.0 ]);
            ];
        }
        (from_json test_multLeagues) );
    ( "noneMiddle_test" >:: fun _ ->
      assert_equal
        {
          league =
            [
              (1, "La Liga");
              (2, "La Liga");
              (3, "La Liga");
              (4, "Premier League");
              (5, "Ligue One");
            ];
          game =
            [
              (1, ("Barca", "Madrid"));
              (2, ("Madrid", "Athletico"));
              (3, ("Barca", "Athletico"));
              (4, ("Chelsea", "NewCastle"));
              (5, ("PSG", "Nice"));
            ];
          site =
            [
              (1, "games.com");
              (2, "games.com");
              (3, "games.com");
              (4, "games.com");
              (5, "games.com");
            ];
          odds =
            [
              (1, Some [ -182.0; 400.0; 320.0 ]);
              (2, Some [ -165.0; 425.0; 370.0 ]);
              (3, None);
              (4, Some [ -170.0; 400.0; 370.0 ]);
              (5, Some [ -170.0; 400.0; 370.0 ]);
            ];
        }
        (from_json test_noneMiddle) );
    ( "noneLast_test" >:: fun _ ->
      assert_equal
        {
          league =
            [
              (1, "La Liga");
              (2, "La Liga");
              (3, "La Liga");
              (4, "Premier League");
              (5, "Ligue One");
            ];
          game =
            [
              (1, ("Barca", "Madrid"));
              (2, ("Madrid", "Athletico"));
              (3, ("Barca", "Athletico"));
              (4, ("Chelsea", "NewCastle"));
              (5, ("PSG", "Nice"));
            ];
          site =
            [
              (1, "games.com");
              (2, "games.com");
              (3, "games.com");
              (4, "games.com");
              (5, "games.com");
            ];
          odds =
            [
              (1, Some [ -182.0; 400.0; 320.0 ]);
              (2, Some [ -165.0; 425.0; 370.0 ]);
              (3, Some [ -170.0; 400.0; 370.0 ]);
              (4, Some [ -170.0; 400.0; 370.0 ]);
              (5, None);
            ];
        }
        (from_json test_noneLast) );
    ( "multLeaguesNone_test" >:: fun _ ->
      assert_equal
        {
          league =
            [
              (1, "La Liga");
              (2, "La Liga");
              (3, "La Liga");
              (4, "Premier League");
              (5, "Ligue One");
            ];
          game =
            [
              (1, ("Barca", "Madrid"));
              (2, ("Madrid", "Athletico"));
              (3, ("Barca", "Athletico"));
              (4, ("Chelsea", "NewCastle"));
              (5, ("PSG", "Nice"));
            ];
          site =
            [
              (1, "games.com");
              (2, "games.com");
              (3, "games.com");
              (4, "games.com");
              (5, "games.com");
            ];
          odds =
            [
              (1, None);
              (2, Some [ -165.0; 425.0; 370.0 ]);
              (3, Some [ -170.0; 400.0; 370.0 ]);
              (4, Some [ -170.0; 400.0; 370.0 ]);
              (5, Some [ -170.0; 400.0; 370.0 ]);
            ];
        }
        (from_json test_multLeaguesNone) );
    ( "multSites_test" >:: fun _ ->
      assert_equal
        {
          league =
            [
              (1, "La Liga");
              (2, "La Liga");
              (3, "La Liga");
              (4, "Premier League");
              (5, "Ligue One");
            ];
          game =
            [
              (1, ("Barca", "Madrid"));
              (2, ("Madrid", "Athletico"));
              (3, ("Barca", "Athletico"));
              (4, ("Chelsea", "NewCastle"));
              (5, ("PSG", "Nice"));
            ];
          site =
            [
              (1, "games.com");
              (2, "games.com");
              (3, "games.com");
              (4, "games.org");
              (5, "games.org");
            ];
          odds =
            [
              (1, None);
              (2, Some [ -165.0; 425.0; 370.0 ]);
              (3, Some [ -170.0; 400.0; 370.0 ]);
              (4, Some [ -170.0; 400.0; 370.0 ]);
              (5, Some [ -170.0; 400.0; 370.0 ]);
            ];
        }
        (from_json test_multSites) );
    ( "uneven_test" >:: fun _ ->
      assert_equal
        {
          league = [ (1, "La Liga") ];
          game = [ (1, ("Barca", "Madrid")); (2, ("Madrid", "Athletico")) ];
          site = [ (1, "games.com"); (2, "games.com"); (3, "games.com") ];
          odds =
            [
              (1, Some [ -170.0; 400.0; 400.0 ]);
              (2, Some [ 1.0; -1.0; 1.0 ]);
              (3, None);
              (4, Some [ 7.0; -2.0; 4.0 ]);
            ];
        }
        (from_json test_uneven) );
  ]

let command_test =
  [
    ( "trivial_print_test" >:: fun _ ->
      assert_equal
        (Print (American, [ "mem"; "lal" ]))
        (parse "print american MEM LAL") );
    ( "printSpongebobMeme_test" >:: fun _ ->
      assert_equal
        (Print (American, [ "mem"; "lal" ]))
        (parse "PrInT american MEM LAL") );
    ( "printUppercase_test" >:: fun _ ->
      assert_equal
        (Print (American, [ "mem"; "lal" ]))
        (parse "Print american MEM LAL") );
    ( "multspacesfirst_print_test" >:: fun _ ->
      assert_equal
        (Print (American, [ "mem"; "lal" ]))
        (parse "print                    american MEM LAL") );
    ( "multspacesmiddle_print_test" >:: fun _ ->
      assert_equal
        (Print (American, [ "mem"; "lal" ]))
        (parse "print american                     MEM LAL") );
    ( "multspaceslast_print_test" >:: fun _ ->
      assert_equal
        (Print (American, [ "mem"; "lal" ]))
        (parse "print american MEM                             LAL") );
    ( "multspacesALL_print_test" >:: fun _ ->
      assert_equal
        (Print (American, [ "mem"; "lal" ]))
        (parse
           "print                 american                  MEM            LAL")
    );
    ( "trivial_decimal_print_test" >:: fun _ ->
      assert_equal
        (Print (Decimal, [ "mem"; "lal" ]))
        (parse "print decimal MEM LAL") );
    ( "decimalSpongebobMeme_test" >:: fun _ ->
      assert_equal
        (Print (Decimal, [ "mem"; "lal" ]))
        (parse "print DeCiMaL MEM LAL") );
    ( "decimalUppercase_test" >:: fun _ ->
      assert_equal
        (Print (Decimal, [ "mem"; "lal" ]))
        (parse "Print Decimal MEM LAL") );
    ( "multspacesbefore_decimal_test" >:: fun _ ->
      assert_equal
        (Print (Decimal, [ "mem"; "lal" ]))
        (parse "print                    decimal MEM LAL") );
    ( "multspacesafter_decimal_test" >:: fun _ ->
      assert_equal
        (Print (Decimal, [ "mem"; "lal" ]))
        (parse "print decimal                     MEM LAL") );
    ( "multspacesALL_decimal_test" >:: fun _ ->
      assert_equal
        (Print (Decimal, [ "mem"; "lal" ]))
        (parse
           "print                 Decimal                  MEM            LAL")
    );
    ( "trivial_calculate_test" >:: fun _ ->
      assert_equal
        (Calculate [ "mem"; "lal"; "pointsbet" ])
        (parse "calculate MEM LAL PointsBet") );
    ( "calculateSpongebobMeme_test" >:: fun _ ->
      assert_equal
        (Calculate [ "mem"; "lal"; "pointsbet" ])
        (parse "CaLcUlAtE  MEM LAL PointsBet") );
    ( "calculateUppercase_test" >:: fun _ ->
      assert_equal
        (Calculate [ "mem"; "lal"; "pointsbet" ])
        (parse "Calculate MEM LAL PointsBet") );
    ( "multspacesfirst_calculate_test" >:: fun _ ->
      assert_equal
        (Calculate [ "mem"; "lal"; "pointsbet" ])
        (parse "calculate                      MEM LAL PointsBet") );
    ( "multspacesmiddle1_calculate_test" >:: fun _ ->
      assert_equal
        (Calculate [ "mem"; "lal"; "pointsbet" ])
        (parse "calculate                  MEM LAL PointsBet") );
    ( "multspacesmiddle2_calculate_test" >:: fun _ ->
      assert_equal
        (Calculate [ "mem"; "lal"; "pointsbet" ])
        (parse "calculate MEM                      LAL PointsBet") );
    ( "multspaceslast_calculate_test" >:: fun _ ->
      assert_equal
        (Calculate [ "mem"; "lal"; "pointsbet" ])
        (parse "calculate MEM LAL                        PointsBet") );
    ( "multspacesALL_calculate_test" >:: fun _ ->
      assert_equal
        (Calculate [ "mem"; "lal"; "pointsbet" ])
        (parse
           "calculate                                MEM            \
            LAL           PointsBet") );
    ( "calculateempty_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "Calculate") );
    ( "calculateOnlySpaces_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "Calculate               ") );
    ("empty_test" >:: fun _ -> assert_raises Empty (fun () -> parse ""));
    ( "misspelled_calculate_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "calculat MEM LAL PointsBet") );
    ( "misspelled_print_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "prin american MEM LAL") );
    ( "missingverb_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "american MEM LAL") );
    ( "quitwithtail_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "quit hello") );
    ( "quitwithhead_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "hello       quit") );
    ("quit_test" >:: fun _ -> assert_equal Quit (parse "quit"));
    ("quitUpperCase_test" >:: fun _ -> assert_equal Quit (parse "Quit"));
    ("quitspacesafter_test" >:: fun _ -> assert_equal Quit (parse "quit       "));
    ( "quitspacesfirst_test" >:: fun _ ->
      assert_equal Quit (parse "          quit") );
    ( "quitspacesboth_test" >:: fun _ ->
      assert_equal Quit (parse "          quit          ") );
    ( "quitmisspelled_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "qui") );
    ("quitSpongebobMeme_test" >:: fun _ -> assert_equal Quit (parse "QuIt"));
    ( "arbitragespacesfirst_test" >:: fun _ ->
      assert_equal (Arbitrage 100.) (parse "          arbitrage 100") );
    ( "arbitragespacesafter_test" >:: fun _ ->
      assert_equal (Arbitrage 100.) (parse "arbitrage 100              ") );
    ( "arbitragespacesboth_test" >:: fun _ ->
      assert_equal (Arbitrage 100.)
        (parse "          arbitrage            100  ") );
    ( "arbitrage_test" >:: fun _ ->
      assert_equal (Arbitrage 100.) (parse "arbitrage 100") );
    ( "arbitrageUppercase_test" >:: fun _ ->
      assert_equal (Arbitrage 100.) (parse "Arbitrage 100") );
    ( "arbitragewithtail_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "arbitrage hello") );
    ( "arbitragewithhead_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "hello       arbitrage") );
    ( "arbitragemisspelled_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "arbitrag") );
    ( "arbitrageSpongebobMeme_test" >:: fun _ ->
      assert_equal (Arbitrage 100.) (parse "ArBiTrAgE 100") );
    ("start_test" >:: fun _ -> assert_equal Start (parse "start"));
    ( "startspacesfirst_test" >:: fun _ ->
      assert_equal Start (parse "          start") );
    ( "startspacesafter_test" >:: fun _ ->
      assert_equal Start (parse "start              ") );
    ( "startspacesboth_test" >:: fun _ ->
      assert_equal Start (parse "          start              ") );
    ("startUppercase_test" >:: fun _ -> assert_equal Start (parse "Start"));
    ( "startwithtail_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "start hello") );
    ( "startwithhead_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "hello       start") );
    ( "startmisspelled_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "star") );
    ("startSpongebobMeme_test" >:: fun _ -> assert_equal Start (parse "StArT"));
    ("home_test" >:: fun _ -> assert_equal Home (parse "home"));
    ( "homespacesfirst_test" >:: fun _ ->
      assert_equal Home (parse "          home") );
    ( "homespacesafter_test" >:: fun _ ->
      assert_equal Home (parse "home              ") );
    ( "homespacesboth_test" >:: fun _ ->
      assert_equal Home (parse "          home              ") );
    ("homeUppercase_test" >:: fun _ -> assert_equal Home (parse "Home"));
    ( "homewithtail_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "home hello") );
    ( "homewithhead_test" >:: fun _ ->
      assert_raises Malformed (fun () -> parse "hello       home") );
    ( "homemisspelled_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "hom") );
    ("homeSpongebobMeme_test" >:: fun _ -> assert_equal Home (parse "HoMe"));
    ( "trivial_goto_test" >:: fun _ ->
      assert_equal (Goto NBA) (parse "go to NBA") );
    ( "gotoSpongebobMeme_test" >:: fun _ ->
      assert_equal (Goto NBA) (parse "Go tO NBA") );
    ( "gotoUppercase_test" >:: fun _ ->
      assert_equal (Goto NBA) (parse "Go To NBA") );
    ( "multspacesfirst_goto_test" >:: fun _ ->
      assert_equal (Goto NBA) (parse "go to                     NBA") );
    ( "goto_withnospace_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "goto Bundesliga") );
    ( "goto_withmanyspaces_test" >:: fun _ ->
      assert_equal (Goto NBA) (parse "go         to NBA") );
    ("goto_NHL_test" >:: fun _ -> assert_equal (Goto NHL) (parse "go to NHL"));
    ("goto_MLB_test" >:: fun _ -> assert_equal (Goto MLB) (parse "go to MLB"));
    ( "goto_BundesLiga_test" >:: fun _ ->
      assert_equal (Goto Bundesliga) (parse "go to Bundesliga") );
    ( "goto_BundesLiga_withspace_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "go to Bundes liga") );
    ( "goto_championsleague_test" >:: fun _ ->
      assert_equal (Goto ChampionsLeague) (parse "go to Champions League") );
    ( "goto_BundesLiga_nospace_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "go to ChampionsLeague") );
    ( "goto_EuropaLeague_test" >:: fun _ ->
      assert_equal (Goto EuropaLeague) (parse "go to Europa League") );
    ( "goto_EuropaLeague_nospace_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "go to EuropaLeague") );
    ( "goto_SerieA_test" >:: fun _ ->
      assert_equal (Goto SerieA) (parse "go to Serie A") );
    ( "goto_SerieA_nospace_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "go to SerieA") );
    ( "goto_LaLiga_test" >:: fun _ ->
      assert_equal (Goto LaLiga) (parse "go to La Liga") );
    ( "goto_LaLiga_nospace_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "go to LaLiga") );
    ( "goto_Ligue1_test" >:: fun _ ->
      assert_equal (Goto Ligue1) (parse "go to Ligue 1") );
    ( "goto_Ligue1_nospace_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "go to Ligue1") );
    ("goto_MLS_test" >:: fun _ -> assert_equal (Goto MLS) (parse "go to MLS"));
    ( "goto_PremierLigue_test" >:: fun _ ->
      assert_equal (Goto PremierLeague) (parse "go to Premier League") );
    ( "goto_PremierLigue_withspace_test" >:: fun _ ->
      assert_raises Malformed (fun _ -> parse "go to PremierLigue") );
    ( "goto_General_test" >:: fun _ ->
      assert_equal (Goto General) (parse "go to General") );
    ( "trivial_help_test" >:: fun _ ->
      assert_equal (Helpwith ArbitrageExpl) (parse "help with arbitrage") );
    ( "helpSpongebobMeme_test" >:: fun _ ->
      assert_equal (Helpwith ArbitrageExpl) (parse "hElP wItH arbitrage") );
    ( "arbitrageSpongebobMeme_test" >:: fun _ ->
      assert_equal (Helpwith ArbitrageExpl) (parse "help WiTh aRbItRaGe") );
    ( "helpUppercase_test" >:: fun _ ->
      assert_equal (Helpwith ArbitrageExpl) (parse "Help With arbitrage") );
    ( "multspacesbefore_help_test" >:: fun _ ->
      assert_equal (Helpwith ArbitrageExpl)
        (parse "                    help with arbitrage") );
    ( "multspacesafter_help_test" >:: fun _ ->
      assert_equal (Helpwith ArbitrageExpl)
        (parse "help       with             arbitrage") );
    ( "trivial_format_help_test" >:: fun _ ->
      assert_equal (Helpwith OddsFormatExpl) (parse "help with odds") );
    ( "formatSpongebobMeme_test" >:: fun _ ->
      assert_equal (Helpwith OddsFormatExpl) (parse "help with oDdS") );
    ( "formatUppercase_test" >:: fun _ ->
      assert_equal (Helpwith OddsFormatExpl) (parse "help With Odds") );
    ( "format_spacesAfter_test" >:: fun _ ->
      assert_equal (Helpwith OddsFormatExpl)
        (parse "help with odds                ") );
    ( "trivial_conv_help_test" >:: fun _ ->
      assert_equal (Helpwith ConvExpl) (parse "help with conversion") );
    ( "conversionSpongebobMeme_test" >:: fun _ ->
      assert_equal (Helpwith ConvExpl) (parse "help wItH CoNvErSiOn") );
    ( "conversionUppercase_test" >:: fun _ ->
      assert_equal (Helpwith ConvExpl) (parse "help With Conversion") );
    ( "conversion_spacesAfter_test" >:: fun _ ->
      assert_equal (Helpwith ConvExpl)
        (parse "help with conversion                ") );
    ( "trivial_prob_help_test" >:: fun _ ->
      assert_equal (Helpwith ProbabilityExpl) (parse "help with probability") );
    ( "probabilitySpongebobMeme_test" >:: fun _ ->
      assert_equal (Helpwith ProbabilityExpl) (parse "help wiTh PrObAbIliTy") );
    ( "probUppercase_test" >:: fun _ ->
      assert_equal (Helpwith ProbabilityExpl) (parse "help With Probability") );
    ( "prob_spacesAfter_test" >:: fun _ ->
      assert_equal (Helpwith ProbabilityExpl)
        (parse "help with probability                ") );
  ]

let suite =
  "test suite for sports betting"
  >::: List.flatten [ translate_tests; command_test ]

let _ = run_test_tt_main suite
