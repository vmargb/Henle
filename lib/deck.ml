type t = { cards : Card.t list; next_id : int }

let empty = { cards = []; next_id = 1 }

let add_card deck ~sentence ~translation ~notes ~source ~now =
  let card = Card.make ~id:deck.next_id ~sentence ~translation ~notes ~source ~now in
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

let sort_by_due cards =
  List.sort (fun (a : Card.t) (b : Card.t) -> compare a.next_review b.next_review) cards

let sort_by_id cards =
  List.sort (fun (a : Card.t) (b : Card.t) -> compare a.id b.id) cards
