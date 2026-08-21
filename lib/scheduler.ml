(* Spaced-repetition scheduling, driven by *felt intuition* rather than
   right/wrong recall. It returns the updated progress.

   Operates on Progress.t rather than Card.t so any drillable thing (sentence
   cards, grammar transition rows, etc.) can be scheduled identically. *)

type review_rating = Easy | Good | Hard

let rating_to_string = function Easy -> "Easy" | Good -> "Good" | Hard -> "Hard"

let rating_of_char = function
  | 'e' | 'E' -> Some Easy
  | 'g' | 'G' -> Some Good
  | 'h' | 'H' -> Some Hard
  | _ -> None

let day = 86400.0

(* cards don't pile up on the same days. Applied as a symmetric +/- fraction of the interval. *)
let jitter_fraction = 0.10

(* [jitter_roll] is a fresh random float in [0.0, 1.0)
   Intervals of 2 days or less are left alone *)
let scheduled_next_review (now : float) (days : int) (jitter_roll : float) : float =
  let base = float_of_int days *. day in
  if days <= 2 then now +. base
  else
    let delta = base *. jitter_fraction *. ((jitter_roll *. 2.0) -. 1.0) in
    now +. base +. delta

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
   Drilling/Fuzzy card to Intuitive via ordinary review. 
   cards that took a long grind need more confirmation that the feeling
   has actually stuck before trusting it. *)
let promote_threshold_for (p : Progress.t) : int =
  match Progress.average_drill_reps p with
  | None -> 2 (* no drill history yet (like an old deck file); use the old default *)
  | Some avg when avg <= 4.0 -> 1
  | Some avg when avg <= 10.0 -> 2
  | Some _ -> 3

(* The demotion logic: how many consecutive Hard reviews in a row
   it takes to send an Intuitive card back to Fuzzy (back into drilling).
   A sentence that took a long grind to click in the first place gets more
   benefit of the doubt on a single rough review, one Hard rating might
   doesn't mean the intuition is gone.
   A sentence that clicked almost instantly getting Hard once is a
   much stronger signal something's actually faded, so it demotes right
   away. *)
let demote_threshold_for (p : Progress.t) : int =
  match Progress.average_drill_reps p with
  | None -> 1 (* no drill history yet, demote right away like before *)
  | Some avg when avg <= 4.0 -> 1
  | Some avg when avg <= 10.0 -> 2
  | Some _ -> 3

let schedule_review (p : Progress.t) (rating : review_rating) (now : float)
    (jitter_roll : float) : Progress.t =
  let open Progress in
  if p.status = Mastered then
    (* Mastered items aren't normally reviewed, but if one comes up (e.g.
       through random sampling) and it suddenly feels Hard, that's a real
       signal that the intuition has decayed and it needs drilling again. *)
    if rating = Hard then
      {
        p with
        status = Fuzzy;
        streak = 0;
        hard_streak = 0;
        last_review = Some now;
        interval_days = 1;
        next_review = scheduled_next_review now 1 jitter_roll;
      }
    else
      {
        p with
        last_review = Some now;
        next_review =
          scheduled_next_review now (max_interval_by_importance p.importance) jitter_roll;
      }
  else
    let current_idx = rung_index_for p.interval_days in
    let idx = next_rung_index current_idx rating in
    let raw_interval = rungs.(idx) in
    let capped_interval = min raw_interval (max_interval_by_importance p.importance) in
    let streak = match rating with Easy | Good -> p.streak + 1 | Hard -> 0 in
    let hard_streak = match rating with Hard -> p.hard_streak + 1 | Easy | Good -> 0 in
    let status =
      match (p.status, rating) with
      | (Drilling | Fuzzy), (Easy | Good) when streak >= promote_threshold_for p ->
          Intuitive
      | Intuitive, Hard when hard_streak >= demote_threshold_for p -> Fuzzy
      | s, _ -> s
    in
    {
      p with
      status;
      streak;
      hard_streak;
      last_review = Some now;
      next_review = scheduled_next_review now capped_interval jitter_roll;
      interval_days = capped_interval;
    }
