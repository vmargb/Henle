type t = { cards : Card.t list; next_id : int }

let empty = { cards = []; next_id = 1 }

let add_card deck ~language ~sentence ~translation ~notes ~source ~now =
  let card = Card.make ~id:deck.next_id ~language ~sentence ~translation ~notes ~source ~now in
  ({ cards = deck.cards @ [ card ]; next_id = deck.next_id + 1 }, card)

let find deck id = List.find_opt (fun (c : Card.t) -> c.id = id) deck.cards

let update deck (updated : Card.t) =
  {
    deck with
    cards =
      List.map
        (fun (c : Card.t) -> if c.id = updated.Card.id then updated else c)
        deck.cards;
  }

(* Cards that still need Henle-style repetition drilling before they're
   ready to enter normal SRS rotation. *)
let drillable deck =
  List.filter
    (fun (c : Card.t) ->
      match c.status with
      | Card.New | Card.Drilling | Card.Fuzzy -> true
      | Card.Intuitive | Card.Mastered -> false)
    deck.cards

(* Cards due for an ordinary SRS review *)
let due_for_review deck now =
  List.filter
    (fun (c : Card.t) ->
      (match c.status with
      | Card.Fuzzy | Card.Intuitive -> true
      | Card.New | Card.Drilling | Card.Mastered -> false)
      && c.next_review <= now)
    deck.cards

let by_status deck status_opt =
  match status_opt with
  | None -> deck.cards
  | Some s -> List.filter (fun (c : Card.t) -> c.status = s) deck.cards

(* Restricts a list of cards to one language. [None] means "all languages". *)
let filter_by_language cards lang_opt =
  match lang_opt with
  | None -> cards
  | Some lang -> List.filter (fun (c : Card.t) -> Card.same_language c.Card.language lang) cards

(* Distinct languages present in the deck, in the display casing of their
   first occurrence, sorted alphabetically *)
let languages deck =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (c : Card.t) ->
      let key = String.lowercase_ascii c.Card.language in
      if not (Hashtbl.mem seen key) then Hashtbl.add seen key c.Card.language)
    deck.cards;
  Hashtbl.fold (fun _ display acc -> display :: acc) seen []
  |> List.sort (fun a b -> compare (String.lowercase_ascii a) (String.lowercase_ascii b))

(* How many cards exist for a given language, split into "still drilling"
   and "in review rotation or mastered" for a quick overview. *)
let count_for_language deck lang =
  List.filter (fun (c : Card.t) -> Card.same_language c.Card.language lang) deck.cards
  |> List.length

(* The language of the most recently added card, used as the default when
   starting a new add session None if the deck is empty. *)
let last_used_language deck =
  match List.sort (fun (a : Card.t) b -> compare b.Card.id a.Card.id) deck.cards with
  | c :: _ -> Some c.Card.language
  | [] -> None

let sort_by_due cards =
  List.sort (fun (a : Card.t) (b : Card.t) -> compare a.next_review b.next_review) cards

(* Cards that have historically needed more reps to click are surfaced
   first in a drill session, so the sentences still resisting intuition
   get your attention. Cards with no drill history yet fall back
   to a neutral mid-range estimate so they mix in naturally rather than
   always being pushed to the back. *)
let neutral_drill_estimate = 5.0

let drill_priority (c : Card.t) : float =
  match Card.average_drill_reps c with
  | Some avg -> avg
  | None -> neutral_drill_estimate

let sort_by_drill_priority cards =
  List.sort
    (fun (a : Card.t) (b : Card.t) ->
      match compare (drill_priority b) (drill_priority a) with
      | 0 -> compare a.Card.next_review b.Card.next_review
      | c -> c)
    cards

let sort_by_id cards =
  List.sort (fun (a : Card.t) (b : Card.t) -> compare a.id b.id) cards
