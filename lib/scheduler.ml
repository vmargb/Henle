(* Spaced-repetition scheduling, driven by *felt intuition* rather than
   right/wrong recall. It returns the updated card. No I/O happens here. *)

type review_rating = Easy | Good | Hard

let rating_to_string = function Easy -> "Easy" | Good -> "Good" | Hard -> "Hard"

let rating_of_char = function
  | 'e' | 'E' -> Some Easy
  | 'g' | 'G' -> Some Good
  | 'h' | 'H' -> Some Hard
  | _ -> None

let day = 86400.0

(* The interval ladder used once a card has had at least one real review *)
let rungs = [| 1; 3; 7; 14; 30; 60; 120 |]

let max_interval_by_importance = function
  | 0 -> 120
  | 1 -> 90
  | 2 -> 60
  | 3 -> 45
  | _ -> 120

(* Index of the largest rung <= interval_days, or -1 *)
let rung_index_for interval_days =
  let idx = ref (-1) in
  Array.iteri (fun i v -> if v <= interval_days then idx := i) rungs;
  !idx

let next_rung_index current_idx rating =
  match rating with
  | Easy -> min (current_idx + 2) (Array.length rungs - 1)
  | Good -> min (current_idx + 1) (Array.length rungs - 1)
  | Hard -> max (current_idx - 2) 0

(* How many consecutive Easy/Good reviews it takes to promote a
   Drilling/Fuzzy card to Intuitive via ordinary review *)
let promote_threshold = 2

let schedule_review (c : Card.t) (rating : review_rating) (now : float) :
    Card.t =
  let open Card in
  if c.status = Mastered then
    (* Mastered cards aren't normally reviewed, but if one comes up (e.g.
       through random sampling) and it suddenly feels Hard, that's a real
       signal that the intuition has decayed and it needs drilling again. *)
    if rating = Hard then
      {
        c with
        status = Fuzzy;
        streak = 0;
        last_review = Some now;
        interval_days = 1;
        next_review = now +. day;
      }
    else
      {
        c with
        last_review = Some now;
        next_review =
          now +. (float_of_int (max_interval_by_importance c.importance) *. day);
      }
  else
    let current_idx = rung_index_for c.interval_days in
    let idx = next_rung_index current_idx rating in
    let raw_interval = rungs.(idx) in
    let capped_interval = min raw_interval (max_interval_by_importance c.importance) in
    let streak = match rating with Easy | Good -> c.streak + 1 | Hard -> 0 in
    let status =
      match (c.status, rating) with
      | (Drilling | Fuzzy), (Easy | Good) when streak >= promote_threshold ->
          Intuitive
      | Intuitive, Hard -> Fuzzy
      | s, _ -> s
    in
    {
      c with
      status;
      streak;
      last_review = Some now;
      next_review = now +. (float_of_int capped_interval *. day);
      interval_days = capped_interval;
    }
