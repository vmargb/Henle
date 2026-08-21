(* A sentence card and its lifecycle status. *)

type t = {
  id : int;
  language : string;
  sentence : string;
  translation : string;
  notes : string option;
  source : string option;
  progress : Progress.t;
}

(* Language used for cards loaded from old deck files that predate the
   language field. *)
let unknown_language = "Unspecified"

(* Case-insensitive language comparison, since user may type "japanese"
   one day and "Japanese" the next. *)
let same_language a b = String.lowercase_ascii a = String.lowercase_ascii b

let make ~id ~language ~sentence ~translation ~notes ~source ~now =
  { id; language; sentence; translation; notes; source; progress = Progress.make ~now }

(* lifetime average reps-per-attempt, or "how hard is this
   sentence historically" *)
let average_drill_reps (c : t) : float option = Progress.average_drill_reps c.progress

let truncate n s = if String.length s <= n then s else String.sub s 0 (n - 1) ^ "\xe2\x80\xa6"
