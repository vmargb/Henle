(* A sentence card and its lifecycle status. *)

type status =
  | New       (* just mined, not drilled yet *)
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
  | s -> failwith ("Unknown card status in deck file: " ^ s)

let status_of_string_opt = function
  | "New" | "new" -> Some New
  | "Drilling" | "drilling" -> Some Drilling
  | "Fuzzy" | "fuzzy" -> Some Fuzzy
  | "Intuitive" | "intuitive" -> Some Intuitive
  | "Mastered" | "mastered" -> Some Mastered
  | _ -> None

type t = {
  id : int;
  language : string;
  sentence : string;
  translation : string;
  notes : string option;
  source : string option;
  status : status;
  difficulty : int; (* 0-3, how hard it feels *)
  importance : int; (* 0-3, how important/useful it is *)
  last_review : float option; (* Unix time, None if never reviewed *)
  next_review : float; (* Unix time when it's due *)
  interval_days : int; (* current interval, in days *)
  streak : int; (* consecutive Easy/Good reviews *)
}

(* Language used for cards loaded from old deck files that predate the
   language field. *)
let unknown_language = "Unspecified"

(* Case-insensitive language comparison, since users may type "japanese"
   one day and "Japanese" the next. *)
let same_language a b = String.lowercase_ascii a = String.lowercase_ascii b

let make ~id ~language ~sentence ~translation ~notes ~source ~now =
  {
    id;
    language;
    sentence;
    translation;
    notes;
    source;
    status = New;
    difficulty = 0;
    importance = 0;
    last_review = None;
    next_review = now;
    (* 0 means "never scheduled", it is distinct from the *)
    interval_days = 0;
    streak = 0;
  }

let truncate n s = if String.length s <= n then s else String.sub s 0 (n - 1) ^ "\xe2\x80\xa6"
