(* Deck file loading/saving. Storage is a plain JSON file (deck.json) *)

let card_to_json (c : Card.t) : Json.t =
  Json.Assoc
    [
      ("id", Json.Number (float_of_int c.id));
      ("language", Json.String c.language);
      ("sentence", Json.String c.sentence);
      ("translation", Json.String c.translation);
      ("notes", (match c.notes with None -> Json.Null | Some s -> Json.String s));
      ("source", (match c.source with None -> Json.Null | Some s -> Json.String s));
      ("progress", Progress.to_json c.progress);
    ]

let card_of_json (j : Json.t) : Card.t =
  {
    Card.id = Json.to_int (Json.member "id" j);
    (* Old deck files fall back safely instead of crashing on load. *)
    language =
      (match Json.member "language" j with
      | Json.Null -> Card.unknown_language
      | v -> Json.to_str v);
    sentence = Json.to_str (Json.member "sentence" j);
    translation = Json.to_str (Json.member "translation" j);
    notes = Json.to_str_opt (Json.member "notes" j);
    source = Json.to_str_opt (Json.member "source" j);
    progress =
      (match Json.member "progress" j with
      | Json.Null ->
          (* Old deck files (pre-Progress refactor) had these fields
             flat directly on the card, read them from there instead. *)
          {
            Progress.status = Progress.status_of_string (Json.to_str (Json.member "status" j));
            difficulty = Json.to_int (Json.member "difficulty" j);
            importance = Json.to_int (Json.member "importance" j);
            last_review = Json.to_float_opt (Json.member "last_review" j);
            next_review = Json.to_float (Json.member "next_review" j);
            interval_days = Json.to_int (Json.member "interval_days" j);
            streak = Json.to_int (Json.member "streak" j);
            hard_streak = (match Json.member "hard_streak" j with Json.Null -> 0 | v -> Json.to_int v);
            drill_attempts = (match Json.member "drill_attempts" j with Json.Null -> 0 | v -> Json.to_int v);
            total_drill_reps = (match Json.member "total_drill_reps" j with Json.Null -> 0 | v -> Json.to_int v);
            last_drill_reps = Json.to_int_opt (Json.member "last_drill_reps" j);
          }
      | pj -> Progress.of_json pj);
  }

let deck_to_json (d : Deck.t) : Json.t =
  Json.Assoc
    [
      ("next_id", Json.Number (float_of_int d.next_id));
      ("cards", Json.List (List.map card_to_json d.cards));
    ]

let deck_of_json (j : Json.t) : Deck.t =
  {
    Deck.next_id = Json.to_int (Json.member "next_id" j);
    cards = List.map card_of_json (Json.to_list (Json.member "cards" j));
  }

let load_deck (path : string) : Deck.t =
  if Sys.file_exists path then (
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    if String.trim s = "" then Deck.empty else deck_of_json (Json.parse s))
  else Deck.empty

let ensure_parent_dir path =
  let dir = Filename.dirname path in
  let rec mkdir_p d =
    if d = "." || d = "/" || Sys.file_exists d then ()
    else (
      mkdir_p (Filename.dirname d);
      try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  mkdir_p dir

let save_deck (path : string) (d : Deck.t) : unit =
  ensure_parent_dir path;
  let j = deck_to_json d in
  let s = Json.to_string j in
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(* grammar store (grammar.json, kept separate from the
   sentence deck since the two have independent lifecycles) *)

let form_to_json ((structure, form) : string * string) : Json.t =
  Json.Assoc [ ("structure", Json.String structure); ("form", Json.String form) ]

let form_of_json (j : Json.t) : string * string =
  (Json.to_str (Json.member "structure" j), Json.to_str (Json.member "form" j))

let row_to_json (r : Grammar.row) : Json.t =
  Json.Assoc
    [
      ("person", Json.String r.Grammar.person);
      ("forms", Json.List (List.map form_to_json r.Grammar.forms));
      ("progress", Progress.to_json r.Grammar.progress);
    ]

let row_of_json (j : Json.t) : Grammar.row =
  {
    Grammar.person = Json.to_str (Json.member "person" j);
    forms = List.map form_of_json (Json.to_list (Json.member "forms" j));
    progress = Progress.of_json (Json.member "progress" j);
  }

let gset_to_json (s : Grammar.set) : Json.t =
  Json.Assoc
    [
      ("id", Json.Number (float_of_int s.Grammar.id));
      ("language", Json.String s.Grammar.language);
      ("lemma", Json.String s.Grammar.lemma);
      ("gloss", (match s.Grammar.gloss with None -> Json.Null | Some g -> Json.String g));
      ("kind", Json.String s.Grammar.kind);
      ("structures", Json.List (List.map (fun st -> Json.String st) s.Grammar.structures));
      ("rows", Json.List (List.map row_to_json s.Grammar.rows));
    ]

let gset_of_json (j : Json.t) : Grammar.set =
  {
    Grammar.id = Json.to_int (Json.member "id" j);
    language = Json.to_str (Json.member "language" j);
    lemma = Json.to_str (Json.member "lemma" j);
    gloss = Json.to_str_opt (Json.member "gloss" j);
    kind = Json.to_str (Json.member "kind" j);
    structures = List.map Json.to_str (Json.to_list (Json.member "structures" j));
    rows = List.map row_of_json (Json.to_list (Json.member "rows" j));
  }

let gstore_to_json (st : Grammar.store) : Json.t =
  Json.Assoc
    [
      ("next_id", Json.Number (float_of_int st.Grammar.next_id));
      ("sets", Json.List (List.map gset_to_json st.Grammar.sets));
    ]

let gstore_of_json (j : Json.t) : Grammar.store =
  {
    Grammar.next_id = Json.to_int (Json.member "next_id" j);
    sets = List.map gset_of_json (Json.to_list (Json.member "sets" j));
  }

let load_grammar (path : string) : Grammar.store =
  if Sys.file_exists path then (
    let ic = open_in_bin path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    if String.trim s = "" then Grammar.empty else gstore_of_json (Json.parse s))
  else Grammar.empty

let save_grammar (path : string) (st : Grammar.store) : unit =
  ensure_parent_dir path;
  let s = Json.to_string (gstore_to_json st) in
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc
