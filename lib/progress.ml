(* The scheduling-relevant state of a drillable thing, how far along it is
   in the Henle pipeline (New -> Drilling/Fuzzy -> Intuitive -> Mastered)

   Factored out of Card so that anything else worth drilling (grammar
   transitions, or anything else) can be scheduled with the exact
   same intuition-based SRS logic, instead of a parallel copy *)

type status =
  | New       (* just added, not drilled yet *)
  | Drilling  (* currently being drilled *)
  | Fuzzy     (* drilled but not intuitive yet *)
  | Intuitive (* had the "aha", now in normal SRS rotation *)
  | Mastered  (* stable long-term, reviewed rarely *)

let status_to_string = function
  | New -> "New"
  | Drilling -> "Drilling"
  | Fuzzy -> "Fuzzy"
  | Intuitive -> "Intuitive"
  | Mastered -> "Mastered"

let status_of_string = function
  | "New" -> New
  | "Drilling" -> Drilling
  | "Fuzzy" -> Fuzzy
  | "Intuitive" -> Intuitive
  | "Mastered" -> Mastered
  | s -> failwith ("Unknown status in deck file: " ^ s)

let status_of_string_opt = function
  | "New" | "new" -> Some New
  | "Drilling" | "drilling" -> Some Drilling
  | "Fuzzy" | "fuzzy" -> Some Fuzzy
  | "Intuitive" | "intuitive" -> Some Intuitive
  | "Mastered" | "mastered" -> Some Mastered
  | _ -> None

type t = {
  status : status;
  difficulty : int; (* 0-3, how hard it feels *)
  importance : int; (* 0-3, how important/useful it is *)
  last_review : float option; (* Unix time, None if never reviewed *)
  next_review : float; (* Unix time when it's due *)
  interval_days : int; (* current interval, in days *)
  streak : int; (* consecutive Easy/Good reviews *)
  hard_streak : int; (* consecutive Hard reviews, same as [streak] for demotion *)
  drill_attempts : int; (* how many times this has been drilled *)
  total_drill_reps : int; (* lifetime sum of reps across every drill attempt *)
  last_drill_reps : int option; (* reps taken on the most recent attempt *)
}

let make ~now =
  {
    status = New;
    difficulty = 0;
    importance = 0;
    last_review = None;
    next_review = now;
    (* 0 means "never scheduled", it is distinct from the 1-day rung *)
    interval_days = 0;
    streak = 0;
    hard_streak = 0;
    drill_attempts = 0;
    total_drill_reps = 0;
    last_drill_reps = None;
  }

(* lifetime average reps-per-attempt, or "how hard is this
   historically" signal *)
let average_drill_reps (p : t) : float option =
  if p.drill_attempts = 0 then None
  else Some (float_of_int p.total_drill_reps /. float_of_int p.drill_attempts)

let to_json (p : t) : Json.t =
  Json.Assoc
    [
      ("status", Json.String (status_to_string p.status));
      ("difficulty", Json.Number (float_of_int p.difficulty));
      ("importance", Json.Number (float_of_int p.importance));
      ("last_review", (match p.last_review with None -> Json.Null | Some f -> Json.Number f));
      ("next_review", Json.Number p.next_review);
      ("interval_days", Json.Number (float_of_int p.interval_days));
      ("streak", Json.Number (float_of_int p.streak));
      ("hard_streak", Json.Number (float_of_int p.hard_streak));
      ("drill_attempts", Json.Number (float_of_int p.drill_attempts));
      ("total_drill_reps", Json.Number (float_of_int p.total_drill_reps));
      ("last_drill_reps", (match p.last_drill_reps with None -> Json.Null | Some n -> Json.Number (float_of_int n)));
    ]

let of_json (j : Json.t) : t =
  {
    status = status_of_string (Json.to_str (Json.member "status" j));
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
