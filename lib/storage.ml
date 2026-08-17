(* Deck file loading/saving. Storage is a plain JSON file (deck.json) *)

let card_to_json (c : Card.t) : Json.t =
  Json.Assoc
    [
      ("id", Json.Number (float_of_int c.id));
      ("sentence", Json.String c.sentence);
      ("translation", Json.String c.translation);
      ("notes", (match c.notes with None -> Json.Null | Some s -> Json.String s));
      ("source", (match c.source with None -> Json.Null | Some s -> Json.String s));
      ("status", Json.String (Card.status_to_string c.status));
      ("difficulty", Json.Number (float_of_int c.difficulty));
      ("importance", Json.Number (float_of_int c.importance));
      ("last_review", (match c.last_review with None -> Json.Null | Some f -> Json.Number f));
      ("next_review", Json.Number c.next_review);
      ("interval_days", Json.Number (float_of_int c.interval_days));
      ("streak", Json.Number (float_of_int c.streak));
    ]

let card_of_json (j : Json.t) : Card.t =
  {
    Card.id = Json.to_int (Json.member "id" j);
    sentence = Json.to_str (Json.member "sentence" j);
    translation = Json.to_str (Json.member "translation" j);
    notes = Json.to_str_opt (Json.member "notes" j);
    source = Json.to_str_opt (Json.member "source" j);
    status = Card.status_of_string (Json.to_str (Json.member "status" j));
    difficulty = Json.to_int (Json.member "difficulty" j);
    importance = Json.to_int (Json.member "importance" j);
    last_review = Json.to_float_opt (Json.member "last_review" j);
    next_review = Json.to_float (Json.member "next_review" j);
    interval_days = Json.to_int (Json.member "interval_days" j);
    streak = Json.to_int (Json.member "streak" j);
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
