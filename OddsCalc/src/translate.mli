(** Creates representation of data from sports betting website Covers.com.

    This module contains the data stored in out.json. It handles loading of that
    data from JSON and translates the data into an OCaml type. *)

type t = {
  league : (int * string) list;
  game : (int * (string * string)) list;
  site : (int * string) list;
  odds : (int * float list option) list;
}
(** type [t] contains the league, game, sites, and odds, indexed with
    an int into an association list. The four record fields reflect the
    structure of the data in our produced "data/out.json."*)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the Translate.t representation of [j]. Requires: [j] is a
    valid JSON League representation *)
