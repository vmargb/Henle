(* Horizontal grammar drilling.

   A [set] is one lemma (like a verb) in one language "amare",
   "hablar", whatever. Forms are added incrementally, one at a time, *as the
   learner meets them*, not filled in all at once from a grammar table

   The row is the drill unit. Drilling never walks a row in a fixed
   sequence (present -> imperfect -> future -> ...) because a fixed order
   just teaches the order, not the ability to jump to any tense on demand
   and feel the change.
   Instead each rep samples a fresh random pair of known structures for
   that person and drills the transition between them. Over many sessions
   every pair gets sampled, in both directions, in no predictable order,
   which is the actual "movement" of horizontal drilling.

   Each row carries its own Progress.t and is scheduled by the exact same
   Scheduler used for sentence cards. *)

type row = {
  person : string; (* "I", "you (sg)", "he/she/it", ... *)
  forms : (string * string) list; (* structure -> surface form, sparse *)
  progress : Progress.t;
}

type set = {
  id : int;
  language : string;
  lemma : string; (* dictionary form, e.g. "amare" *)
  gloss : string option; (* meaning, e.g. "to love" *)
  kind : string; (* free text: "verb", "noun declension", etc *)
  structures : string list; (* every structure/tense encountered so far,
                                in first-entered order; used for display
                                only, never for drill order *)
  rows : row list;
}

type store = { sets : set list; next_id : int }

let empty : store = { sets = []; next_id = 1 }

let same_language = Card.same_language

(* A row needs at least two filled-in forms before there's a transition to
   drill at all. *)
let transition_eligible (r : row) : bool = List.length r.forms >= 2

let make_row ~person ~now : row = { person; forms = []; progress = Progress.make ~now }

let make_set ~id ~language ~lemma ~gloss ~kind : set =
  { id; language; lemma; gloss; kind; structures = []; rows = [] }

let add_set store ~language ~lemma ~gloss ~kind : store * set =
  let s = make_set ~id:store.next_id ~language ~lemma ~gloss ~kind in
  ({ sets = store.sets @ [ s ]; next_id = store.next_id + 1 }, s)

let find_set store id = List.find_opt (fun (s : set) -> s.id = id) store.sets

let update_set store (updated : set) =
  { store with sets = List.map (fun (s : set) -> if s.id = updated.id then updated else s) store.sets }

let find_set_by_lemma store ~language ~lemma =
  List.find_opt
    (fun (s : set) -> same_language s.language language && String.lowercase_ascii s.lemma = String.lowercase_ascii lemma)
    store.sets

let find_row (s : set) person = List.find_opt (fun (r : row) -> r.person = person) s.rows

(* Adds or overwrites a single (person, structure) -> form cell. Creates
   the row (with a fresh Progress.t) if this is a new person for the set,
   and appends the structure to the set's known list if new. Safe to call
   repeatedly over time when meeting new forms. *)
let add_form (s : set) ~person ~structure ~form ~now : set =
  let structures = if List.mem structure s.structures then s.structures else s.structures @ [ structure ] in
  let rows =
    if List.exists (fun (r : row) -> r.person = person) s.rows then
      List.map
        (fun (r : row) ->
          if r.person = person then
            { r with forms = (structure, form) :: List.remove_assoc structure r.forms }
          else r)
        s.rows
    else s.rows @ [ { (make_row ~person ~now) with forms = [ (structure, form) ] } ]
  in
  { s with structures; rows }

let update_row (s : set) (updated : row) : set =
  { s with rows = List.map (fun (r : row) -> if r.person = updated.person then updated else r) s.rows }

(* Keeps [structures] limited to labels still actually used by some row,
   so a deleted form's tense doesn't linger forever in listings/suggestions. *)
let recompute_structures (s : set) : set =
  let still_used st = List.exists (fun (r : row) -> List.mem_assoc st r.forms) s.rows in
  { s with structures = List.filter still_used s.structures }

(* Removes one (person, structure) cell. If that was the person's last
   known form, the row itself is removed too (an empty row can't be
   drilled and just clutters listings). *)
let remove_form (s : set) ~person ~structure : set =
  match find_row s person with
  | None -> s
  | Some r ->
      let forms' = List.remove_assoc structure r.forms in
      let rows' =
        if forms' = [] then List.filter (fun (rr : row) -> rr.person <> person) s.rows
        else List.map (fun (rr : row) -> if rr.person = person then { rr with forms = forms' } else rr) s.rows
      in
      recompute_structures { s with rows = rows' }

(* Removes an entire person's row, and every form under it. *)
let remove_row (s : set) ~person : set =
  recompute_structures { s with rows = List.filter (fun (r : row) -> r.person <> person) s.rows }

(* Removes an entire lemma/set from the store. *)
let remove_set (store : store) id : store = { store with sets = List.filter (fun (s : set) -> s.id <> id) store.sets }

(* Picks two distinct, random (structure, form) pairs from a row's known
   forms, in a random order (so drilling covers both A->B and B->A over
   time, not just one fixed direction). Requires transition_eligible. *)
let pick_transition (r : row) : (string * string) * (string * string) =
  let arr = Array.of_list r.forms in
  let n = Array.length arr in
  let i = Random.int n in
  let j =
    let rec pick () =
      let k = Random.int n in
      if k = i then pick () else k
    in
    pick ()
  in
  (arr.(i), arr.(j))

(*  queries across the whole store, mirroring Decks shape *)

(* (set, row) pairs still in the "building the shape" phase, New, or
   drilled-but-not-yet-Intuitive, and with at least one transition *)
let drillable store =
  List.concat_map
    (fun (s : set) ->
      List.filter_map
        (fun (r : row) ->
          if
            transition_eligible r
            &&
            match r.progress.Progress.status with
            | Progress.New | Progress.Drilling | Progress.Fuzzy -> true
            | Progress.Intuitive | Progress.Mastered -> false
          then Some (s, r)
          else None)
        s.rows)
    store.sets

let due_for_review store now =
  List.concat_map
    (fun (s : set) ->
      List.filter_map
        (fun (r : row) ->
          if
            transition_eligible r
            && (match r.progress.Progress.status with
               | Progress.Fuzzy | Progress.Intuitive -> true
               | Progress.New | Progress.Drilling | Progress.Mastered -> false)
            && r.progress.Progress.next_review <= now
          then Some (s, r)
          else None)
        s.rows)
    store.sets

let filter_by_language (pairs : (set * row) list) lang_opt =
  match lang_opt with
  | None -> pairs
  | Some lang -> List.filter (fun ((s : set), _) -> same_language s.language lang) pairs

let languages store =
  let seen = Hashtbl.create 8 in
  List.iter
    (fun (s : set) ->
      let key = String.lowercase_ascii s.language in
      if not (Hashtbl.mem seen key) then Hashtbl.add seen key s.language)
    store.sets;
  Hashtbl.fold (fun _ display acc -> display :: acc) seen []
  |> List.sort (fun a b -> compare (String.lowercase_ascii a) (String.lowercase_ascii b))

let sets_for_language store lang_opt =
  match lang_opt with
  | None -> store.sets
  | Some lang -> List.filter (fun (s : set) -> same_language s.language lang) store.sets

let row_count (s : set) = List.length s.rows

let average_drill_reps (r : row) : float option = Progress.average_drill_reps r.progress

let neutral_drill_estimate = 5.0

let drill_priority ((_, r) : set * row) : float =
  match average_drill_reps r with Some avg -> avg | None -> neutral_drill_estimate

let sort_by_drill_priority pairs =
  List.sort
    (fun a b ->
      match compare (drill_priority b) (drill_priority a) with
      | 0 -> compare (snd a).progress.Progress.next_review (snd b).progress.Progress.next_review
      | c -> c)
    pairs

let sort_by_due pairs =
  List.sort (fun a b -> compare (snd a).progress.Progress.next_review (snd b).progress.Progress.next_review) pairs

(* known-label suggestions, for autocomplete when adding forms *)

let known_structures_for_language (store : store) (language : string) : string list =
  let seen = Hashtbl.create 8 in
  let order = ref [] in
  List.iter
    (fun (s : set) ->
      if same_language s.language language then
        List.iter
          (fun st ->
            let key = String.lowercase_ascii st in
            if not (Hashtbl.mem seen key) then begin
              Hashtbl.add seen key ();
              order := st :: !order
            end)
          s.structures)
    store.sets;
  List.rev !order

(* Same idea for persons/pronouns, so "he/she/it" stays spelled one
   way across every verb in a language. *)
let known_persons_for_language (store : store) (language : string) : string list =
  let seen = Hashtbl.create 8 in
  let order = ref [] in
  List.iter
    (fun (s : set) ->
      if same_language s.language language then
        List.iter
          (fun (r : row) ->
            let key = String.lowercase_ascii r.person in
            if not (Hashtbl.mem seen key) then begin
              Hashtbl.add seen key ();
              order := r.person :: !order
            end)
          s.rows)
    store.sets;
  List.rev !order
