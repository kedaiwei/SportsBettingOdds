(** Creates representation of data from sports betting website Covers.com

    This module contains the data stored in out.json. It handles loading of that
    data from JSON and translates them into OCaml. *)

type t = {
  league : (int * string) list;
  game : (int * (string * string)) list;
  site : (int * string) list;
  odds : (int * float list option) list;
}
(** type t contains a table form containing odds or porbabilities with their
    associated website, matchup, and sports league. Each row is indexed with an
    int in a pair.*)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the Translate.t representation of [j]. Requires: [j] is a
    valid JSON League representation *)
