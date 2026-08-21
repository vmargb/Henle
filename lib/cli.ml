(* Interactive command-line interface for Henle *)

let now () = Unix.time ()
let () = Random.self_init ()

let default_deck_path () =
  match Sys.getenv_opt "HENLE_DECK" with
  | Some p -> p
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some home -> Filename.concat home ".henle/deck.json"
      | None -> "./henle_deck.json")

(* ---------- small I/O helpers ---------- *)

let print_rule () = print_endline (String.make 60 '-')

let clear_screen () =
  print_string "\027[2J\027[H";
  flush stdout

(* ---------- color ---------- *)

(* only colorize when stdout is an actual terminal *)
let use_color = try Unix.isatty Unix.stdout with _ -> false

let colorize (code : string) (s : string) : string =
  if use_color then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s

(* basic colors *)
let bold s = colorize "1" s
let dim s = colorize "2" s
let underline s = colorize "4" s
let green s = colorize "32" s
let red s = colorize "31" s
let yellow s = colorize "33" s
let blue s = colorize "34" s
let magenta s = colorize "35" s
let cyan s = colorize "36" s
let white s = colorize "37" s
let bright_green s = colorize "92" s
let bright_yellow s = colorize "93" s
let bright_cyan s = colorize "96" s
let bright_red s = colorize "91" s
let bright_magenta s = colorize "95" s

(* semantic aliases *)
let success = green
let warning = yellow
let info = cyan
let error = red
let highlight = bold

(* helpter to repeat a string n times *)
let repeat n s =
  let rec loop acc i = if i >= n then acc else loop (acc ^ s) (i+1) in
  loop "" 0

(* ---------- box drawing and progress ---------- *)

(* Use Unicode box-drawing if color is enabled (proxy for terminal capability)
   otherwise fallback to ASCII. *)
let (ul, h, ur, v, ll, lr) =
  if use_color then
    ("┌", "─", "┐", "│", "└", "┘")
  else
    ("+", "-", "+", "|", "+", "+")

let boxed_text (text : string) : string =
  let width = String.length text + 2 in
  let top = ul ^ repeat width h ^ ur in
  let mid = v ^ " " ^ text ^ " " ^ v in
  let bot = ll ^ repeat width h ^ lr in
  top ^ "\n" ^ mid ^ "\n" ^ bot

let print_boxed_title title =
  print_endline (bold (boxed_text title))

let progress_bar (completed : int) (total : int) (width : int) : string =
  (* Use ASCII fallback if not using color (since Unicode might not be supported) *)
  let (solid, light) =
    if use_color then ("█", "░") else ("#", "-")
  in
  if total = 0 then repeat width light
  else
    let filled = (completed * width) / total in
    let empty = width - filled in
    repeat filled solid ^ repeat empty light

let print_progress (completed : int) (total : int) (label : string) =
  let width = 30 in
  let bar = progress_bar completed total width in
  let pct = if total = 0 then 0 else (completed * 100) / total in
  let label_colored = bold (Printf.sprintf "%-9s" label) in
  Printf.printf "\r\027[K%s [%s] %3d%% (%d/%d)" label_colored bar pct completed total;
  flush stdout

(* ---------- status and language colors ---------- *)

let status_color (status : Progress.status) : string =
  match status with
  | Progress.New -> "90" (* gray *)
  | Progress.Drilling -> "33" (* yellow *)
  | Progress.Fuzzy -> "35" (* magenta *)
  | Progress.Intuitive -> "32" (* green *)
  | Progress.Mastered -> "34" (* blue *)

let colored_status (status : Progress.status) : string =
  colorize (status_color status) (Progress.status_to_string status)

(* color per language *)
let language_palette = [| "36"; "35"; "34"; "31"; "32" |] (* cyan magenta blue red green *)

let language_color (lang : string) : string =
  let h = Hashtbl.hash (String.lowercase_ascii lang) in
  language_palette.(h mod Array.length language_palette)

let colored_language (lang : string) : string = colorize (language_color lang) lang

(* colorizes already width-padded string, so table alignment fixed
   ANSI escape codes are zero-width on screen but still count towards
   String.length, so padding must happen before colorizing, not after *)
let colored_padded (code : string) (padded : string) : string = colorize code padded

exception Stdin_closed

let prompt (label : string) : string =
  print_string label;
  flush stdout;
  try String.trim (read_line ()) with End_of_file -> raise Stdin_closed

let prompt_opt (label : string) : string option =
  match prompt label with "" -> None | s -> Some s

let prompt_opt_default (label : string) (current : string option) : string option =
  let shown = match current with Some s -> " [" ^ s ^ "]" | None -> " [none]" in
  match prompt (label ^ shown ^ ": ") with
  | "" -> current
  | "-" -> None (* explicit "clear this field" *)
  | s -> Some s

let prompt_default (label : string) (current : string) : string =
  match prompt (Printf.sprintf "%s [%s]: " label current) with
  | "" -> current
  | s -> s

let rec prompt_int_default (label : string) (current : int) ~min:lo ~max:hi : int =
  match prompt (Printf.sprintf "%s (%d-%d) [%d]: " label lo hi current) with
  | "" -> current
  | s -> (
      match int_of_string_opt (String.trim s) with
      | Some n when n >= lo && n <= hi -> n
      | _ ->
          print_endline (Printf.sprintf "Please enter a number between %d and %d." lo hi);
          prompt_int_default label current ~min:lo ~max:hi)

let rec prompt_yn ?(default : bool option) (label : string) : bool =
  let suffix = match default with Some true -> " (Y/n): " | Some false -> " (y/N): " | None -> " (y/n): " in
  match String.lowercase_ascii (prompt (label ^ suffix)) with
  | "y" | "yes" -> true
  | "n" | "no" -> false
  | "" when default <> None -> Option.get default
  | _ ->
      print_endline "Please answer y or n.";
      prompt_yn ?default label

let format_date (t : float) : string =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

(* Blocks until the user is ready to move on *)
let wait_for_continue () = ignore (prompt (dim "\nPress Enter to return to the menu: "))

(* ---------- shared lookups ---------- *)

let get_card_or_fail deck id =
  match Deck.find deck id with
  | Some c -> c
  | None ->
      Printf.printf "No card with id %d.\n" id;
      exit 1

let parse_id_arg (s : string) : int =
  match int_of_string_opt s with
  | Some n -> n
  | None ->
      Printf.printf "Expected a numeric card id, got '%s'.\n" s;
      exit 1

(* ---------- flag parsing ---------- *)

(* Used for --lang and --status, which can
   show up before or after other positional arguments. *)
let extract_flag (flag : string) (args : string list) : string option * string list =
  let rec loop acc = function
    | f :: v :: rest when f = flag -> (Some v, List.rev_append acc rest)
    | x :: rest -> loop (x :: acc) rest
    | [] -> (None, List.rev acc)
  in
  loop [] args

(* ---------- card display ---------- *)

let show_card_full (c : Card.t) =
  Printf.printf "%s: %d\n" (dim "id") c.id;
  Printf.printf "%s: %s\n" (dim "language") (colored_language c.language);
  Printf.printf "%s: %s\n" (dim "status") (colored_status c.progress.Progress.status);
  Printf.printf "%s: %s\n" (dim "sentence") c.sentence;
  Printf.printf "%s: %s\n" (dim "translation") c.translation;
  (match c.notes with Some n -> Printf.printf "%s: %s\n" (dim "notes") n | None -> ());
  (match c.source with Some s -> Printf.printf "%s: %s\n" (dim "source") s | None -> ());
  Printf.printf "%s: %d/3\n" (dim "difficulty") c.progress.Progress.difficulty;
  Printf.printf "%s: %d/3\n" (dim "importance") c.progress.Progress.importance;
  Printf.printf "%s: %d\n" (dim "streak") c.progress.Progress.streak;
  Printf.printf "%s: %d day(s)\n" (dim "interval") c.progress.Progress.interval_days;
  (match c.progress.Progress.last_review with
  | Some t -> Printf.printf "%s: %s\n" (dim "last review") (format_date t)
  | None -> Printf.printf "%s: never\n" (dim "last review"));
  Printf.printf "%s: %s\n" (dim "next review") (format_date c.progress.Progress.next_review);
  (match Card.average_drill_reps c with
  | Some avg ->
      Printf.printf "%s: %d attempt(s), %.1f avg reps%s\n" (dim "drill history") c.progress.Progress.drill_attempts avg
        (match c.progress.Progress.last_drill_reps with Some n -> Printf.sprintf " (last: %d)" n | None -> "")
  | None -> Printf.printf "%s: not drilled yet\n" (dim "drill history"))

let list_row (c : Card.t) =
  let lang_col = colored_padded (language_color c.language) (Printf.sprintf "%-12s" (Card.truncate 12 c.language)) in
  let status_col = colored_padded (status_color c.progress.Progress.status) (Printf.sprintf "%-10s" (Progress.status_to_string c.progress.Progress.status)) in
  Printf.printf "%-4d %s %-44s %s %1d/3  %1d/3  %5dd  %s\n" c.id lang_col
    (Card.truncate 44 c.sentence)
    status_col
    c.progress.Progress.difficulty c.progress.Progress.importance c.progress.Progress.interval_days
    (format_date c.progress.Progress.next_review)

let list_header () =
  Printf.printf "%s %s %s %s %s %s %s  %s\n"
    (bold "ID") (bold "LANG") (bold "SENTENCE")
    (bold "STATUS") (bold "DIFF") (bold "IMP") (bold "INTVL") (bold "NEXT REVIEW");
  print_rule ()

let status_legend () =
  print_endline
    (Printf.sprintf "  %s = not drilled yet   %s/%s = still working on it   \
     %s = clicked, in review rotation   %s = rarely reviewed"
       (colored_status Progress.New) (colored_status Progress.Drilling) (colored_status Progress.Fuzzy)
       (colored_status Progress.Intuitive) (colored_status Progress.Mastered))

(* ---------- add ---------- *)

let prompt_one_sentence () =
  let sentence =
    prompt (info "Sentence (in the language you're learning), or leave blank to stop: ")
  in
  if sentence = "" then None
  else begin
    let translation = prompt (info "  Your translation of it: ") in
    let notes = prompt_opt (info "  Any grammar or usage notes? (optional, Enter to skip): ") in
    let source = prompt_opt (info "  Where's it from? (optional, Enter to skip): ") in
    Some (sentence, translation, notes, source)
  end

(* Determines the language for an add session. [lang_arg] is an explicit *)
let prompt_language ?preferred_default (deck : Deck.t) (lang_arg : string option) : string =
  match lang_arg with
  | Some l -> l
  | None -> (
      let default =
        match preferred_default with Some p -> Some p | None -> Deck.last_used_language deck
      in
      match default with
      | Some d -> prompt_default (info "Language for this batch (e.g. Latin, Japanese)") d
      | None ->
          let rec ask () =
            match prompt (info "Language for this batch (e.g. Latin, Japanese): ") with
            | "" ->
                print_endline "Please enter a language.";
                ask ()
            | s -> s
          in
          ask ())

(* Adds one or more sentences interactively, all in the same language.
   Returns the ids that were added, so the menu, or this function itself
   can offer to drill that batch together right away. *)
let cmd_add path (lang_arg : string option) (preferred_default : string option) : int list =
  let is_first_run = not (Sys.file_exists path) in
  if is_first_run then begin
    print_endline (bold "Welcome to Henle.");
    print_endline "";
    print_endline "The idea: you don't memorize grammar, you drill them";
    print_endline "read one over and over until the meaning just lands, with no";
    print_endline "English in your head. Once it clicks, spaced review keeps";
    print_endline "that feeling fresh over time. Let's add your first sentence.";
    print_newline ()
  end
  else begin
    print_endline (info "Adding sentence(s).");
    print_endline "Add as many as you like, one at a time";
    print_endline "they'll be offered as a single drill session once you're done.";
    print_newline ()
  end;
  let deck = ref (Storage.load_deck path) in
  let language = prompt_language ?preferred_default !deck lang_arg in
  Printf.printf "\nAdding sentence(s) in: %s\n\n" (colored_language language);
  let added_ids = ref [] in
  let rec loop () =
    match prompt_one_sentence () with
    | None -> ()
    | Some (sentence, translation, notes, source) ->
        let deck', card =
          Deck.add_card !deck ~language ~sentence ~translation ~notes ~source ~now:(now ())
        in
        deck := deck';
        Storage.save_deck path !deck;
        added_ids := card.Card.id :: !added_ids;
        Printf.printf "  -> %s #%d.\n\n" (success "saved as") card.Card.id;
        loop ()
  in
  loop ();
  List.rev !added_ids

(* ---------- drilling ---------- *)

let drill_intro n =
  Printf.printf "%s\n" (bold (Printf.sprintf "%d sentence(s) to drill." n));
  print_endline "This isn't a test, there's no wrong answer. Read or say the sentence,";
  print_endline "then press Enter to repeat it again. Each Enter counts as one rep.";
  print_endline (Printf.sprintf "When it clicks, type %s. If you want to give up on this one for now," (green "'y'"));
  print_endline (Printf.sprintf "type %s, the rep count still gets saved." (red "'g'"));
  print_newline ()

(* Drills a single card: shows it, then loops incrementing a rep counter
   on every blank Enter, until the person types 'y' (it clicked) or 'g'
   (giving up for now). Returns (aha, reps), reps is saved either way,
   since even a failed attempt is informative about how hard this
   sentence is. *)
let rec drill_loop rep : bool * int =
  (* Redraw in place instead of printing a new line every rep *)
  Printf.printf "\r\027[K  [rep %d] Enter to repeat, %s if it clicked, %s to give up: " rep (green "'y'") (red "'g'");
  flush stdout;
  match (try String.trim (read_line ()) with End_of_file -> raise Stdin_closed) with
  | "" ->
      print_string "\027[1A";
      drill_loop (rep + 1)
  | s -> (
      match String.lowercase_ascii s with
      | "y" | "yes" -> (true, rep)
      | "g" | "give" -> (false, rep)
      | _ ->
          print_string "\027[1A\027[K";
          print_endline (warning "  Please press Enter, or type 'y' or 'g'.");
          drill_loop rep)

let run_drill_on ?lang path (cards : Card.t list) =
  if cards = [] then
    print_endline
      (match lang with
      | Some l -> Printf.sprintf "Nothing to drill in %s right now." l
      | None -> "Nothing to drill right now, every sentence is either intuitive or mastered.")
  else begin
    let deck = ref (Storage.load_deck path) in
    let total = List.length cards in
    drill_intro total;
    print_progress 0 total "Drilling";
    print_newline ();  (* move to next line after progress bar *)
    List.iteri
      (fun i (c : Card.t) ->
        print_progress (i+1) total "Drilling";
        print_newline ();
        print_rule ();
        Printf.printf "%s %d of %d\n" (bold "Card") (i + 1) total;
        Printf.printf "#%d  [%s]  (%s)\n" c.Card.id (colored_status c.Card.progress.Progress.status) (colored_language c.Card.language);
        Printf.printf "  %s\n" c.Card.sentence;
        (match c.Card.notes with Some n -> Printf.printf "  notes: %s\n" n | None -> ());
        print_newline ();
        let skip = String.lowercase_ascii (prompt (info "Press Enter to drill this now, or type 's' to skip it: ")) = "s" in
        if not skip then begin
          let aha, reps = drill_loop 1 in
          let with_stats (p : Progress.t) =
            {
              p with
              Progress.drill_attempts = p.Progress.drill_attempts + 1;
              total_drill_reps = p.Progress.total_drill_reps + reps;
              last_drill_reps = Some reps;
            }
          in
          let new_progress =
            if aha then
              (* A quick click is a stronger signal than a long grind, so it
                 earns a slightly longer first interval before review. *)
              let initial_interval = if reps <= 3 then 2 else 1 in
              with_stats
                {
                  c.Card.progress with
                  Progress.status = Progress.Intuitive;
                  interval_days = initial_interval;
                  next_review = now () +. (float_of_int initial_interval *. Scheduler.day);
                }
            else
              with_stats
                {
                  c.Card.progress with
                  Progress.status =
                    (if c.Card.progress.Progress.status = Progress.New then Progress.Drilling
                     else Progress.Fuzzy);
                }
          in
          let updated = { c with Card.progress = new_progress } in
          deck := Deck.update !deck updated;
          Storage.save_deck path !deck;
          let avg_note =
            match Card.average_drill_reps updated with
            | Some avg when updated.Card.progress.Progress.drill_attempts > 1 ->
                Printf.sprintf " (%d reps this time, %.1f avg over %d attempts)" reps avg
                  updated.Card.progress.Progress.drill_attempts
            | _ -> Printf.sprintf " (%d rep%s)" reps (if reps = 1 then "" else "s")
          in
          print_endline
            ((if aha then success "-> nice, that's marked as Intuitive." else warning "-> no problem, it'll come back in your next drill session.")
            ^ avg_note)
        end;
        print_newline ())
      cards;
    print_progress total total "Drilling";
    print_newline ();
    print_endline (success "Drilling session complete.")
  end

let cmd_drill path limit (lang_opt : string option) =
  let deck = Storage.load_deck path in
  let all = Deck.sort_by_drill_priority (Deck.filter_by_language (Deck.drillable deck) lang_opt) in
  let total = List.length all in
  let candidates =
    match limit with
    | None -> all
    | Some n -> List.filteri (fun i _ -> i < n) all
  in
  if candidates <> [] && List.length candidates < total then
    Printf.printf "(%s %d more waiting after this session, run `henle drill` again to keep going.)\n\n"
      (info "note:") (total - List.length candidates);
  run_drill_on ?lang:lang_opt path candidates

(* ---------- review ---------- *)

let default_review_limit = 20

let cmd_review path (limit : int option) (lang_opt : string option) =
  let deck = ref (Storage.load_deck path) in
  let t = now () in
  let due_all = Deck.sort_by_due (Deck.filter_by_language (Deck.due_for_review !deck t) lang_opt) in
  if due_all = [] then
    print_endline
      (match lang_opt with
      | Some l -> Printf.sprintf "No sentences due for review in %s right now." l
      | None -> "No sentences due for review right now.")
  else begin
    let total_due = List.length due_all in
    let cap = match limit with Some n -> n | None -> default_review_limit in
    let due = if total_due > cap then List.filteri (fun i _ -> i < cap) due_all else due_all in
    let total = List.length due in
    if total < total_due then
      Printf.printf "%s %d sentence(s) due, showing the %d most overdue.\n" (info "note:") total_due total
    else Printf.printf "%s\n" (bold (Printf.sprintf "%d sentence(s) due." total));
    print_endline "This is a quick check-in, not a test of memory: for each sentence,";
    print_endline "rate how direct and intuitive it feels *right now*. 'Hard' just";
    print_endline "means it needs more drilling again, it isn't a failure.";
    if total < total_due then
      Printf.printf "(%d more waiting, run `henle review` again to keep going.)\n" (total_due - total);
    print_newline ();
    print_progress 0 total_due "Reviewing";
    print_newline ();
    List.iteri
      (fun i (c : Card.t) ->
        print_progress (i+1) total_due "Reviewing";
        print_newline ();
        print_rule ();
        Printf.printf "%s %d of %d\n" (bold "Card") (i + 1) total;
        Printf.printf "#%d  (%s)\n" c.Card.id (colored_language c.Card.language);
        Printf.printf "  %s\n" c.Card.sentence;
        print_newline ();
        let rec ask () =
          match
            prompt (Printf.sprintf "How does it feel? (%s, %s, %s, %s): "
                      (green "e = Easy/direct")
                      (yellow "g = Good/mostly direct")
                      (red "h = Hard/still translating")
                      (dim "s = skip"))
          with
          | "s" | "S" -> None
          | "" -> ask ()
          | s -> (
              match Scheduler.rating_of_char s.[0] with
              | Some r -> Some r
              | None ->
                  print_endline "Please enter e, g, h, or s.";
                  ask ())
        in
        match ask () with
        | None -> print_newline ()
        | Some rating ->
            Printf.printf "  %s: %s\n" (dim "translation") c.Card.translation;
            let new_progress = Scheduler.schedule_review c.Card.progress rating t (Random.float 1.0) in
            let updated = { c with Card.progress = new_progress } in
            deck := Deck.update !deck updated;
            Storage.save_deck path !deck;
            let rating_msg =
              match rating with
              | Scheduler.Easy -> success "Easy"
              | Scheduler.Good -> info "Good"
              | Scheduler.Hard -> warning "Hard"
            in
            Printf.printf "  -> %s. Next review: %s. (status: %s)\n"
              rating_msg
              (format_date updated.Card.progress.Progress.next_review)
              (colored_status updated.Card.progress.Progress.status);
            if updated.Card.progress.Progress.status = Progress.Intuitive && updated.Card.progress.Progress.streak >= 5 then begin
              if prompt_yn (warning "  This one's felt easy for a while. Mark it Mastered (review it much less often)?") then begin
                let interval = Scheduler.max_interval_by_importance updated.Card.progress.Progress.importance in
                let mastered =
                  {
                    updated with
                    Card.progress =
                      {
                        updated.Card.progress with
                        Progress.status = Progress.Mastered;
                        interval_days = interval;
                        next_review = t +. (float_of_int interval *. Scheduler.day);
                      };
                  }
                in
                deck := Deck.update !deck mastered;
                Storage.save_deck path !deck;
                print_endline (success "  -> marked Mastered.")
              end
            end;
            print_newline ())
      due;
    print_progress total_due total_due "Reviewing";
    print_newline ();
    print_endline (success "Review session complete.")
  end

(* ---------- list / show / edit / master ---------- *)

let cmd_list path status_filter (lang_opt : string option) =
  let deck = Storage.load_deck path in
  let status_opt =
    match status_filter with
    | None -> None
    | Some s -> (
        match Progress.status_of_string_opt s with
        | Some st -> Some st
        | None ->
            Printf.printf "Unknown status '%s'. Valid: new, drilling, fuzzy, intuitive, mastered.\n" s;
            exit 1)
  in
  let cards = Deck.sort_by_id (Deck.filter_by_language (Deck.by_status deck status_opt) lang_opt) in
  if cards = [] then print_endline "No sentences match."
  else begin
    list_header ();
    List.iter list_row cards;
    print_newline ();
    status_legend ()
  end

let cmd_show path id =
  let deck = Storage.load_deck path in
  let c = get_card_or_fail deck id in
  show_card_full c

let cmd_edit path id =
  let deck = Storage.load_deck path in
  let c = get_card_or_fail deck id in
  print_endline (info "Editing. Press Enter to keep the current value, or '-' to clear an optional field.");
  let language = prompt_default (info "Language") c.Card.language in
  let sentence = prompt_default (info "Sentence") c.Card.sentence in
  let translation = prompt_default (info "Translation") c.Card.translation in
  let notes = prompt_opt_default (info "Notes") c.Card.notes in
  let source = prompt_opt_default (info "Source") c.Card.source in
  let difficulty = prompt_int_default (info "Difficulty (0=easy, 3=hard)") c.Card.progress.Progress.difficulty ~min:0 ~max:3 in
  let importance = prompt_int_default (info "Importance (0=low priority, 3=high)") c.Card.progress.Progress.importance ~min:0 ~max:3 in
  let updated =
    {
      c with
      Card.language;
      sentence;
      translation;
      notes;
      source;
      progress = { c.Card.progress with Progress.difficulty; importance };
    }
  in
  let deck = Deck.update deck updated in
  Storage.save_deck path deck;
  print_endline (success "Saved.")

let cmd_master path id =
  let deck = Storage.load_deck path in
  let c = get_card_or_fail deck id in
  let t = now () in
  let interval = Scheduler.max_interval_by_importance c.Card.progress.Progress.importance in
  let updated =
    {
      c with
      Card.progress =
        {
          c.Card.progress with
          Progress.status = Progress.Mastered;
          interval_days = interval;
          next_review = t +. (float_of_int interval *. Scheduler.day);
        };
    }
  in
  let deck = Deck.update deck updated in
  Storage.save_deck path deck;
  Printf.printf "%s #%d marked Mastered, it'll be reviewed only rarely from now on.\n" (success "Card") id

let cmd_unmaster path id =
  let deck = Storage.load_deck path in
  let c = get_card_or_fail deck id in
  let t = now () in
  let updated =
    {
      c with
      Card.progress =
        {
          c.Card.progress with
          Progress.status = Progress.Intuitive;
          interval_days = 14;
          next_review = t +. (14.0 *. Scheduler.day);
        };
    }
  in
  let deck = Deck.update deck updated in
  Storage.save_deck path deck;
  Printf.printf "%s #%d is back in normal rotation (next review in 14 days).\n" (info "Card") id

let cmd_due path (lang_opt : string option) =
  let deck = Storage.load_deck path in
  let t = now () in
  match lang_opt with
  | Some lang ->
      let drill_n = List.length (Deck.filter_by_language (Deck.drillable deck) lang_opt) in
      let review_n = List.length (Deck.filter_by_language (Deck.due_for_review deck t) lang_opt) in
      Printf.printf "%s: %s ready to drill, %s due for review.\n" (bold lang) (bright_yellow (string_of_int drill_n)) (bright_green (string_of_int review_n))
  | None ->
      let drill_n = List.length (Deck.drillable deck) in
      let review_n = List.length (Deck.due_for_review deck t) in
      Printf.printf "%s: %s ready to drill, %s due for review.\n" (bold "All languages") (bright_yellow (string_of_int drill_n)) (bright_green (string_of_int review_n));
      let langs = Deck.languages deck in
      if List.length langs > 1 then begin
        print_newline ();
        List.iter
          (fun lang ->
            let d = List.length (Deck.filter_by_language (Deck.drillable deck) (Some lang)) in
            let r = List.length (Deck.filter_by_language (Deck.due_for_review deck t) (Some lang)) in
            Printf.printf "  %-15s %s drill  %s review\n" (colored_language lang) (bright_yellow (string_of_int d)) (bright_green (string_of_int r)))
          langs
      end

(* ---------- languages ---------- *)

let cmd_languages path =
  let deck = Storage.load_deck path in
  let langs = Deck.languages deck in
  if langs = [] then print_endline "No sentences yet, `henle add` to mine your first one."
  else begin
    Printf.printf "%-15s %s\n" (bold "LANGUAGE") (bold "CARDS");
    print_rule ();
    List.iter
      (fun lang -> Printf.printf "%-15s %d\n" (colored_language lang) (Deck.count_for_language deck lang))
      langs
  end

(* ---------- grammar drilling ---------- *)

let default_grammar_path () =
  match Sys.getenv_opt "HENLE_GRAMMAR" with
  | Some p -> p
  | None -> (
      match Sys.getenv_opt "HOME" with
      | Some home -> Filename.concat home ".henle/grammar.json"
      | None -> "./henle_grammar.json")

let gram_last_used_language (store : Grammar.store) : string option =
  match List.sort (fun (a : Grammar.set) b -> compare b.id a.id) store.sets with
  | s :: _ -> Some s.Grammar.language
  | [] -> None

let prompt_gram_language ?preferred_default (store : Grammar.store) (lang_arg : string option) : string =
  match lang_arg with
  | Some l -> l
  | None -> (
      let default =
        match preferred_default with Some p -> Some p | None -> gram_last_used_language store
      in
      match default with
      | Some d -> prompt_default (info "Language for this lemma (e.g. Latin, Spanish)") d
      | None ->
          let rec ask () =
            match prompt (info "Language for this lemma (e.g. Latin, Spanish): ") with
            | "" ->
                print_endline "Please enter a language.";
                ask ()
            | s -> s
          in
          ask ())

let gset_label (s : Grammar.set) : string =
  match s.Grammar.gloss with
  | Some g -> Printf.sprintf "%s (%s)" s.Grammar.lemma g
  | None -> s.Grammar.lemma

(* Prompts for a value, but with autocomplete against previously-used
   labels for the current language Falls back to prompt_opt outright if there are no
   known labels yet to suggest. *)
let prompt_with_suggestions (label : string) (suggestions : string list) : string option =
  match suggestions with
  | [] -> prompt_opt (info label)
  | _ ->
      print_string (info label);
      print_newline ();
      List.iteri (fun i s -> Printf.printf "    %s) %s\n" (bold (string_of_int (i + 1))) s) suggestions;
      let rec ask () =
        match prompt "    > (number, new text, or blank to stop): " with
        | "" -> None
        | s -> (
            match int_of_string_opt s with
            | Some i when i >= 1 && i <= List.length suggestions -> Some (List.nth suggestions (i - 1))
            | Some _ ->
                print_endline "    Not a listed number, type the number, matching text, or something new.";
                ask ()
            | None ->
                let low = String.lowercase_ascii s in
                let exact = List.filter (fun sug -> String.lowercase_ascii sug = low) suggestions in
                let prefix_matches =
                  List.filter
                    (fun sug ->
                      let lsug = String.lowercase_ascii sug in
                      String.length low <= String.length lsug && String.sub lsug 0 (String.length low) = low)
                    suggestions
                in
                (match (exact, prefix_matches) with
                | [ e ], _ -> Some e (* same label, keep the existing casing *)
                | _, [ one ] ->
                    Printf.printf "    (autocompleted to %s)\n" (highlight one);
                    Some one
                | _ -> Some s (* no unambiguous match, treat as a new label *)))
      in
      ask ()

let gram_list_header () =
  Printf.printf "%s %s %s %s %s %s %s\n" (bold "ID") (bold "LANG") (bold "LEMMA") (bold "GLOSS")
    (bold "KIND") (bold "PERSONS") (bold "STRUCTURES");
  print_rule ()

let gram_list_row (s : Grammar.set) =
  Printf.printf "%-4d %s %-16s %-20s %-10s %-8d %s\n" s.Grammar.id
    (colored_padded (language_color s.Grammar.language) (Printf.sprintf "%-8s" (Card.truncate 8 s.Grammar.language)))
    (Card.truncate 16 s.Grammar.lemma)
    (Card.truncate 20 (Option.value ~default:"-" s.Grammar.gloss))
    (Card.truncate 10 s.Grammar.kind)
    (Grammar.row_count s)
    (String.concat ", " s.Grammar.structures)

(* Adds a new lemma, or finds an existing one by (language, lemma), then
   loops collecting (person, structure, form) one at a time.
   Saved to disk after every single form, so an interrupted session never
   loses earlier entries *)
let cmd_gram_add path (lang_arg : string option) : unit =
  let store = ref (Storage.load_grammar path) in
  let language = prompt_gram_language !store lang_arg in
  let lemma = prompt (info (Printf.sprintf "Lemma / dictionary form in %s (e.g. amare, hablar), or blank to cancel: " language)) in
  if lemma = "" then print_endline "Nothing added."
  else begin
    let s =
      match Grammar.find_set_by_lemma !store ~language ~lemma with
      | Some existing ->
          Printf.printf "Found an existing entry for %s, adding to it.\n" (highlight (gset_label existing));
          existing
      | None ->
          let gloss = prompt_opt (info "  Gloss / meaning (optional): ") in
          let kind = prompt_default (info "  Kind") "verb" in
          let store', s = Grammar.add_set !store ~language ~lemma ~gloss ~kind in
          store := store';
          Storage.save_grammar path !store;
          s
    in
    print_newline ();
    print_endline "Add forms one at a time as you meet them. Leave the person blank to stop.";
    print_endline "You don't need to fill in every tense for every person right away,";
    print_endline "add what you have; drilling only uses persons with 2+ known forms.";
    print_endline "Person and tense prompts autocomplete against labels you've used before";
    print_endline "in this language, so spelling stays consistent across verbs.";
    print_newline ();
    let rec loop (s : Grammar.set) =
      let persons_sugg = Grammar.known_persons_for_language !store language in
      match prompt_with_suggestions "Person/pronoun (e.g. I, you, he/she/it):" persons_sugg with
      | None -> ()
      | Some person -> (
          let structures_sugg = Grammar.known_structures_for_language !store language in
          match prompt_with_suggestions "  Structure/tense (e.g. Present, Conditional):" structures_sugg with
          | None ->
              print_endline "  Skipped, a structure/tense is required.";
              loop s
          | Some structure -> (
              match prompt_opt (info "  Form: ") with
              | None ->
                  print_endline "  Skipped, a form is required.";
                  loop s
              | Some form ->
                  let s' = Grammar.add_form s ~person ~structure ~form ~now:(now ()) in
                  store := Grammar.update_set !store s';
                  Storage.save_grammar path !store;
                  Printf.printf "  -> %s.\n\n" (success "saved");
                  loop s'))
    in
    loop s
  end

(* ---------- grammar drilling: fixed person, random structure pairs ---------- *)

let gram_drill_intro n =
  Printf.printf "%s\n" (bold (Printf.sprintf "%d grammar row(s) to drill." n));
  print_endline "Each rep shows a random pair of tenses/structures for one person,";
  print_endline "never the same fixed order twice. Read both forms, feel the shift";
  print_endline "between them, then press Enter for a fresh random pair.";
  print_endline (Printf.sprintf "When the movement clicks, type %s. To give up on this one for now," (green "'y'"));
  print_endline (Printf.sprintf "type %s, the rep count still gets saved." (red "'g'"));
  print_newline ()

(* Clears exactly the last [n] terminal rows this process printed *)
let clear_last_lines n = if n > 0 then Printf.printf "\027[%dA\027[J" n

(* One round of transition drilling, pick a fresh random pair, show both
   forms, and ask what to do next *)
let rec gram_round (r : Grammar.row) rep (warn : string option) : bool * int =
  let (a_struct, a_form), (b_struct, b_form) = Grammar.pick_transition r in
  (match warn with Some w -> print_endline (warning ("  " ^ w)) | None -> ());
  Printf.printf "    %s -> %s\n" (bold a_struct) (bold b_struct);
  Printf.printf "      %-15s %s\n" a_struct a_form;
  Printf.printf "      %-15s %s\n" b_struct b_form;
  Printf.printf "  [rep %d] Enter for a new random pair, %s if it clicked, %s to give up: " rep (green "'y'") (red "'g'");
  flush stdout;
  let lines_printed = (match warn with Some _ -> 1 | None -> 0) + 4 in
  match (try String.trim (read_line ()) with End_of_file -> raise Stdin_closed) with
  | "" ->
      clear_last_lines lines_printed;
      gram_round r (rep + 1) None
  | s -> (
      match String.lowercase_ascii s with
      | "y" | "yes" -> (true, rep)
      | "g" | "give" -> (false, rep)
      | _ ->
          clear_last_lines lines_printed;
          gram_round r rep (Some "Please press Enter, or type 'y' or 'g'."))

let gram_drill_loop (r : Grammar.row) rep : bool * int = gram_round r rep None

let run_gram_drill_on ?lang path (pairs : (Grammar.set * Grammar.row) list) =
  if pairs = [] then
    print_endline
      (match lang with
      | Some l -> Printf.sprintf "Nothing to drill in %s right now." l
      | None ->
          "Nothing to drill right now. Add a lemma with `henle gram add`, or add a second\n\
           form to an existing person so there's a transition to drill.")
  else begin
    let store = ref (Storage.load_grammar path) in
    let total = List.length pairs in
    gram_drill_intro total;
    print_progress 0 total "Drilling";
    print_newline ();
    List.iteri
      (fun i ((s : Grammar.set), (r : Grammar.row)) ->
        print_progress (i + 1) total "Drilling";
        print_newline ();
        print_rule ();
        Printf.printf "%s %d of %d\n" (bold "Row") (i + 1) total;
        Printf.printf "%s  %s  [%s]  (%s)\n" (highlight (gset_label s)) r.Grammar.person
          (colored_status r.Grammar.progress.Progress.status) (colored_language s.Grammar.language);
        print_newline ();
        let skip = String.lowercase_ascii (prompt (info "Press Enter to drill this now, or type 's' to skip it: ")) = "s" in
        if not skip then begin
          let aha, reps = gram_drill_loop r 1 in
          let with_stats (p : Progress.t) =
            { p with Progress.drill_attempts = p.drill_attempts + 1; total_drill_reps = p.total_drill_reps + reps; last_drill_reps = Some reps }
          in
          let new_progress =
            if aha then
              let initial_interval = if reps <= 3 then 2 else 1 in
              with_stats
                {
                  r.Grammar.progress with
                  Progress.status = Progress.Intuitive;
                  interval_days = initial_interval;
                  next_review = now () +. (float_of_int initial_interval *. Scheduler.day);
                }
            else
              with_stats
                {
                  r.Grammar.progress with
                  Progress.status =
                    (if r.Grammar.progress.Progress.status = Progress.New then Progress.Drilling else Progress.Fuzzy);
                }
          in
          let updated_row = { r with Grammar.progress = new_progress } in
          let updated_set = Grammar.update_row s updated_row in
          store := Grammar.update_set !store updated_set;
          Storage.save_grammar path !store;
          print_endline
            (if aha then success "-> nice, that movement is marked Intuitive."
             else warning "-> no problem, it'll come back in your next drill session.")
        end;
        print_newline ())
      pairs;
    print_progress total total "Drilling";
    print_newline ();
    print_endline (success "Drilling session complete.")
  end

let cmd_gram_drill path limit (lang_opt : string option) =
  let store = Storage.load_grammar path in
  let all = Grammar.sort_by_drill_priority (Grammar.filter_by_language (Grammar.drillable store) lang_opt) in
  let total = List.length all in
  let candidates = match limit with None -> all | Some n -> List.filteri (fun i _ -> i < n) all in
  if candidates <> [] && List.length candidates < total then
    Printf.printf "(%s %d more waiting after this session, run `henle gram drill` again to keep going.)\n\n"
      (info "note:") (total - List.length candidates);
  run_gram_drill_on ?lang:lang_opt path candidates

(* ---------- grammar review: one random cued transition per row ---------- *)

let cmd_gram_review path (limit : int option) (lang_opt : string option) =
  let store = ref (Storage.load_grammar path) in
  let t = now () in
  let due_all = Grammar.sort_by_due (Grammar.filter_by_language (Grammar.due_for_review !store t) lang_opt) in
  if due_all = [] then
    print_endline
      (match lang_opt with
      | Some l -> Printf.sprintf "No grammar rows due for review in %s right now." l
      | None -> "No grammar rows due for review right now.")
  else begin
    let total_due = List.length due_all in
    let cap = match limit with Some n -> n | None -> default_review_limit in
    let due = if total_due > cap then List.filteri (fun i _ -> i < cap) due_all else due_all in
    let total = List.length due in
    if total < total_due then
      Printf.printf "%s %d row(s) due, showing the %d most overdue.\n" (info "note:") total_due total
    else Printf.printf "%s\n" (bold (Printf.sprintf "%d row(s) due." total));
    print_endline "One random transition per row, cued: you see the first form, rate how";
    print_endline "it feels to jump to the second *before* it's revealed.";
    print_newline ();
    print_progress 0 total_due "Reviewing";
    print_newline ();
    List.iteri
      (fun i ((s : Grammar.set), (r : Grammar.row)) ->
        print_progress (i + 1) total_due "Reviewing";
        print_newline ();
        print_rule ();
        Printf.printf "%s %d of %d\n" (bold "Row") (i + 1) total;
        Printf.printf "%s  %s  (%s)\n" (highlight (gset_label s)) r.Grammar.person (colored_language s.Grammar.language);
        let (a_struct, a_form), (b_struct, _b_form) = Grammar.pick_transition r in
        Printf.printf "  %-15s %s\n" a_struct a_form;
        print_newline ();
        let rec ask () =
          match
            prompt (Printf.sprintf "Jumping to %s from here, how does it feel? (%s, %s, %s, %s): " (highlight b_struct)
                      (green "e = Easy/direct") (yellow "g = Good/mostly direct") (red "h = Hard/still translating") (dim "s = skip"))
          with
          | "s" | "S" -> None
          | "" -> ask ()
          | s -> (
              match Scheduler.rating_of_char s.[0] with
              | Some r -> Some r
              | None ->
                  print_endline "Please enter e, g, h, or s.";
                  ask ())
        in
        (match ask () with
        | None -> print_newline ()
        | Some rating ->
            let _, b_form = List.find (fun (st, _) -> st = b_struct) r.Grammar.forms in
            Printf.printf "  %s: %s\n" (dim b_struct) b_form;
            let new_progress = Scheduler.schedule_review r.Grammar.progress rating t (Random.float 1.0) in
            let updated_row = { r with Grammar.progress = new_progress } in
            let updated_set = Grammar.update_row s updated_row in
            store := Grammar.update_set !store updated_set;
            Storage.save_grammar path !store;
            let rating_msg =
              match rating with Scheduler.Easy -> success "Easy" | Scheduler.Good -> info "Good" | Scheduler.Hard -> warning "Hard"
            in
            Printf.printf "  -> %s. Next review: %s. (status: %s)\n" rating_msg
              (format_date new_progress.Progress.next_review) (colored_status new_progress.Progress.status);
            if new_progress.Progress.status = Progress.Intuitive && new_progress.Progress.streak >= 5 then begin
              if prompt_yn (warning "  This one's felt easy for a while. Mark it Mastered (review it much less often)?") then begin
                let interval = Scheduler.max_interval_by_importance new_progress.Progress.importance in
                let mastered_progress =
                  { new_progress with Progress.status = Progress.Mastered; interval_days = interval; next_review = t +. (float_of_int interval *. Scheduler.day) }
                in
                let mastered_row = { updated_row with Grammar.progress = mastered_progress } in
                store := Grammar.update_set !store (Grammar.update_row updated_set mastered_row);
                Storage.save_grammar path !store;
                print_endline (success "  -> marked Mastered.")
              end
            end;
            print_newline ()))
      due;
    print_progress total_due total_due "Reviewing";
    print_newline ();
    print_endline (success "Review session complete.")
  end

(* ---------- grammar list / show ---------- *)

let cmd_gram_list path (lang_opt : string option) =
  let store = Storage.load_grammar path in
  let sets = Grammar.sets_for_language store lang_opt in
  if sets = [] then print_endline "No grammar sets yet, `henle gram add` to add your first lemma."
  else begin
    gram_list_header ();
    List.iter gram_list_row (List.sort (fun (a : Grammar.set) b -> compare a.id b.id) sets)
  end

let get_gset_or_fail store id =
  match Grammar.find_set store id with
  | Some s -> s
  | None ->
      Printf.printf "No grammar set with id %d.\n" id;
      exit 1

let cmd_gram_show path id =
  let store = Storage.load_grammar path in
  let s = get_gset_or_fail store id in
  Printf.printf "%s: %d\n" (dim "id") s.Grammar.id;
  Printf.printf "%s: %s\n" (dim "language") (colored_language s.Grammar.language);
  Printf.printf "%s: %s\n" (dim "lemma") s.Grammar.lemma;
  (match s.Grammar.gloss with Some g -> Printf.printf "%s: %s\n" (dim "gloss") g | None -> ());
  Printf.printf "%s: %s\n" (dim "kind") s.Grammar.kind;
  print_newline ();
  if s.Grammar.rows = [] then print_endline "No forms added yet."
  else
    List.iter
      (fun (r : Grammar.row) ->
        Printf.printf "%s  [%s]\n" (highlight r.Grammar.person) (colored_status r.Grammar.progress.Progress.status);
        List.iter (fun (structure, form) -> Printf.printf "  %-15s %s\n" structure form) r.Grammar.forms;
        (if not (Grammar.transition_eligible r) then
           print_endline (dim "  (needs a 2nd structure before this can be drilled)"));
        print_newline ())
      s.Grammar.rows

let cmd_gram_due path (lang_opt : string option) =
  let store = Storage.load_grammar path in
  let t = now () in
  match lang_opt with
  | Some lang ->
      let drill_n = List.length (Grammar.filter_by_language (Grammar.drillable store) lang_opt) in
      let review_n = List.length (Grammar.filter_by_language (Grammar.due_for_review store t) lang_opt) in
      Printf.printf "%s: %s ready to drill, %s due for review.\n" (bold lang) (bright_yellow (string_of_int drill_n)) (bright_green (string_of_int review_n))
  | None ->
      let drill_n = List.length (Grammar.drillable store) in
      let review_n = List.length (Grammar.due_for_review store t) in
      Printf.printf "%s: %s ready to drill, %s due for review.\n" (bold "All languages") (bright_yellow (string_of_int drill_n)) (bright_green (string_of_int review_n))

let usage () =
  print_endline (bold "henle, Henle-style sentence drilling with intuition-based SRS");
  print_endline "";
  print_endline "Run `henle` with no arguments for a guided menu. Or use these";
  print_endline "commands directly:";
  print_endline "";
  print_endline (Printf.sprintf "  %s add [--lang LANG]                  add sentence(s) to the deck" (bold "henle"));
  print_endline (Printf.sprintf "  %s drill [N] [--lang LANG]             drilling session: repeat sentences until they click (default: 5)" (bold "henle"));
  print_endline (Printf.sprintf "  %s review [N] [--lang LANG]            review session: rate how intuitive due sentences feel (default cap: 20)" (bold "henle"));
  print_endline (Printf.sprintf "  %s list [--status STATUS] [--lang LANG]  list sentences (new/drilling/fuzzy/intuitive/mastered)" (bold "henle"));
  print_endline (Printf.sprintf "  %s show <id>                           show full details for a sentence" (bold "henle"));
  print_endline (Printf.sprintf "  %s edit <id>                           edit a sentence's fields" (bold "henle"));
  print_endline (Printf.sprintf "  %s master <id>                         suspend a sentence from normal rotation (rarely reviewed)" (bold "henle"));
  print_endline (Printf.sprintf "  %s unmaster <id>                       bring a Mastered sentence back into rotation" (bold "henle"));
  print_endline (Printf.sprintf "  %s due [--lang LANG]                   show counts of what's ready to drill/review" (bold "henle"));
  print_endline (Printf.sprintf "  %s languages                           list languages in the deck, with card counts" (bold "henle"));
  print_endline (Printf.sprintf "  %s help                                show this message" (bold "henle"));
  print_endline "";
  print_endline "--lang filters to one language (case-insensitive). Omit it to work";
  print_endline "across every language at once.";
  print_endline "";
  print_endline "Drilling vs. review, in short:";
  print_endline (Printf.sprintf "  %s = for NEW or still-fuzzy sentences: repeat until it clicks." (bold "drill"));
  print_endline (Printf.sprintf "  %s = for sentences that already clicked: quick check that the feeling stuck." (bold "review"));
  print_endline "";
  print_endline (bold "Grammar drilling (horizontal: random tense-to-tense transitions per person)");
  print_endline (Printf.sprintf "  %s gram add [--lang LANG]                add/extend a lemma's known forms" (bold "henle"));
  print_endline (Printf.sprintf "  %s gram drill [N] [--lang LANG]          drill random structure pairs until they click (default: 5)" (bold "henle"));
  print_endline (Printf.sprintf "  %s gram review [N] [--lang LANG]         rate one cued transition per due row (default cap: 20)" (bold "henle"));
  print_endline (Printf.sprintf "  %s gram list [--lang LANG]               list grammar sets" (bold "henle"));
  print_endline (Printf.sprintf "  %s gram show <id>                        show a set's known forms and per-person status" (bold "henle"));
  print_endline (Printf.sprintf "  %s gram due [--lang LANG]                counts of what's ready to drill/review" (bold "henle"));
  print_endline "";
  print_endline "Forms are added one at a time as you meet them, never a whole table at";
  print_endline "once. A person only enters drill rotation once it has 2+ known forms,";
  print_endline "since a transition needs two tenses to move between. Every drill rep";
  print_endline "samples a fresh random pair (never a fixed present->past->future order)";
  print_endline "so you build the ability to jump to any tense on demand, not just recite";
  print_endline "a memorized sequence.";
  print_endline "";
  Printf.printf "Deck file: %s  (override with $HENLE_DECK)\n" (bold (default_deck_path ()));
  Printf.printf "Grammar file: %s  (override with $HENLE_GRAMMAR)\n" (bold (default_grammar_path ()))

(* guided menu (default entry point) *)

(* Lets the user pick which language to focus on (or all of them). Returns
   the new filter. If the deck has no cards yet, there's nothing to choose
   from, so this just says so and leaves the filter as "all". *)
let choose_language (deck : Deck.t) (current : string option) : string option =
  let langs = Deck.languages deck in
  if langs = [] then begin
    print_endline "No sentences yet, add some first, then you can split by language.";
    None
  end
  else begin
    print_endline "Which language would you like to train?";
    let marker l = if current = Some l then " (current)" else "" in
    Printf.printf "  %s) %s\n" (bold "0") (if current = None then bold "All languages (current)" else "All languages");
    List.iteri
      (fun i l ->
        Printf.printf "  %s) %s (%d card(s))%s\n" (bold (string_of_int (i+1))) (colored_language l) (Deck.count_for_language deck l) (marker l))
      langs;
    let n_langs = List.length langs in
    let rec ask () =
      match prompt "> " with
      | "0" -> None
      | s -> (
          match int_of_string_opt s with
          | Some i when i >= 1 && i <= n_langs -> Some (List.nth langs (i - 1))
          | _ ->
              print_endline "Not a valid option.";
              ask ())
    in
    ask ()
  end

let rec grammar_menu (gpath : string) (lang_filter : string option) =
  clear_screen ();
  let store = Storage.load_grammar gpath in
  let t = now () in
  let drill_n = List.length (Grammar.filter_by_language (Grammar.drillable store) lang_filter) in
  let review_n = List.length (Grammar.filter_by_language (Grammar.due_for_review store t) lang_filter) in
  print_boxed_title "Henle: Grammar Drilling (horizontal)";
  print_newline ();
  Printf.printf "  %s %s\n" (bold "Training:") (match lang_filter with Some l -> colored_language l | None -> bold "All languages");
  print_newline ();
  Printf.printf "  %s ready to drill   (random structure pairs, repeat until it clicks)\n" (bright_yellow (string_of_int drill_n));
  Printf.printf "  %s due for review   (one cued transition, rate how it feels)\n" (bright_green (string_of_int review_n));
  print_newline ();
  print_endline (bold "What would you like to do?");
  print_endline (Printf.sprintf "  %s) Add/extend a lemma's forms" (bold "1"));
  print_endline (Printf.sprintf "  %s) Drill  : random structure-to-structure transitions" (bold "2"));
  print_endline (Printf.sprintf "  %s) Review : rate a cued transition on due rows" (bold "3"));
  print_endline (Printf.sprintf "  %s) List grammar sets" (bold "4"));
  print_endline (Printf.sprintf "  %s) Back to main menu" (dim "b"));
  match String.lowercase_ascii (prompt "> ") with
  | "1" | "add" ->
      clear_screen ();
      cmd_gram_add gpath lang_filter;
      wait_for_continue ();
      grammar_menu gpath lang_filter
  | "2" | "drill" ->
      clear_screen ();
      cmd_gram_drill gpath (Some 5) lang_filter;
      wait_for_continue ();
      grammar_menu gpath lang_filter
  | "3" | "review" ->
      clear_screen ();
      cmd_gram_review gpath None lang_filter;
      wait_for_continue ();
      grammar_menu gpath lang_filter
  | "4" | "list" ->
      clear_screen ();
      cmd_gram_list gpath lang_filter;
      wait_for_continue ();
      grammar_menu gpath lang_filter
  | "b" | "back" -> ()
  | _ ->
      print_endline "Not a valid option, pick a number from the list, or b to go back.";
      grammar_menu gpath lang_filter

let rec interactive_menu path (lang_filter : string option) =
  clear_screen ();
  let deck = Storage.load_deck path in
  let t = now () in
  let drill_n = List.length (Deck.filter_by_language (Deck.drillable deck) lang_filter) in
  let review_n = List.length (Deck.filter_by_language (Deck.due_for_review deck t) lang_filter) in

  print_boxed_title "Henle: An Intuition Drilling Method";
  print_newline ();

  Printf.printf "  %s %s\n" (bold "Training:") (match lang_filter with Some l -> colored_language l | None -> bold "All languages");
  print_newline ();
  Printf.printf "  %s ready to drill   (still building intuition, repeat until it clicks)\n" (bright_yellow (string_of_int drill_n));
  Printf.printf "  %s due for review   (already clicked, check the feeling has stuck)\n" (bright_green (string_of_int review_n));
  print_newline ();

  print_endline (bold "What would you like to do?");
  print_endline (Printf.sprintf "  %s) Add sentence(s)" (bold "1"));
  print_endline (Printf.sprintf "  %s) Drill   : repeat new sentences until they click" (bold "2"));
  print_endline (Printf.sprintf "  %s) Review  : review old sentences that already clicked" (bold "3"));
  print_endline (Printf.sprintf "  %s) List sentences" (bold "4"));
  print_endline (Printf.sprintf "  %s) Full command reference (for scripting/power use)" (bold "5"));
  print_endline (Printf.sprintf "  %s) Grammar drilling (horizontal: random tense-to-tense transitions)" (bold "6"));
  print_endline (Printf.sprintf "  %s) Switch language" (bold "l"));
  print_endline (Printf.sprintf "  %s) Quit" (dim "q"));

  match String.lowercase_ascii (prompt "> ") with
  | "1" | "add" ->
      clear_screen ();
      let ids = cmd_add path None lang_filter in
      if
        ids <> []
        && prompt_yn ~default:true
             (Printf.sprintf "\nDrill these %d new sentence(s) together now?" (List.length ids))
      then begin
        clear_screen ();
        let deck = Storage.load_deck path in
        let cards = List.filter_map (Deck.find deck) ids in
        run_drill_on path cards
      end;
      wait_for_continue ();
      interactive_menu path lang_filter
  | "2" | "drill" ->
      clear_screen ();
      cmd_drill path (Some 5) lang_filter;
      wait_for_continue ();
      interactive_menu path lang_filter
  | "3" | "review" ->
      clear_screen ();
      cmd_review path None lang_filter;
      wait_for_continue ();
      interactive_menu path lang_filter
  | "4" | "list" ->
      clear_screen ();
      cmd_list path None lang_filter;
      wait_for_continue ();
      interactive_menu path lang_filter
  | "5" | "help" ->
      clear_screen ();
      usage ();
      wait_for_continue ();
      interactive_menu path lang_filter
  | "6" | "gram" | "grammar" ->
      grammar_menu (default_grammar_path ()) lang_filter;
      interactive_menu path lang_filter
  | "l" | "lang" | "language" ->
      clear_screen ();
      let lang_filter = choose_language deck lang_filter in
      interactive_menu path lang_filter
  | "q" | "quit" | "exit" -> ()
  | _ ->
      print_endline "Not a valid option, pick a number from the list, or q to quit.";
      interactive_menu path lang_filter

let main_dispatch path =
  clear_screen ();
  match Array.to_list Sys.argv with
  | _ :: "add" :: rest ->
      let lang, _rest = extract_flag "--lang" rest in
      ignore (cmd_add path lang None)
  | _ :: "drill" :: rest ->
      let lang, rest = extract_flag "--lang" rest in
      let n =
        match rest with
        | n :: _ -> ( match int_of_string_opt n with Some n -> n | None -> 5)
        | [] -> 5
      in
      cmd_drill path (Some n) lang
  | _ :: "review" :: rest ->
      let lang, rest = extract_flag "--lang" rest in
      let n = match rest with n :: _ -> int_of_string_opt n | [] -> None in
      cmd_review path n lang
  | _ :: "list" :: rest ->
      let lang, rest = extract_flag "--lang" rest in
      let status, _rest = extract_flag "--status" rest in
      cmd_list path status lang
  | _ :: "show" :: id :: _ -> cmd_show path (parse_id_arg id)
  | _ :: "edit" :: id :: _ -> cmd_edit path (parse_id_arg id)
  | _ :: "master" :: id :: _ -> cmd_master path (parse_id_arg id)
  | _ :: "unmaster" :: id :: _ -> cmd_unmaster path (parse_id_arg id)
  | _ :: "due" :: rest ->
      let lang, _rest = extract_flag "--lang" rest in
      cmd_due path lang
  | _ :: "languages" :: _ -> cmd_languages path
  | _ :: "gram" :: "add" :: rest ->
      let lang, _rest = extract_flag "--lang" rest in
      cmd_gram_add (default_grammar_path ()) lang
  | _ :: "gram" :: "drill" :: rest ->
      let lang, rest = extract_flag "--lang" rest in
      let n = match rest with n :: _ -> (match int_of_string_opt n with Some n -> n | None -> 5) | [] -> 5 in
      cmd_gram_drill (default_grammar_path ()) (Some n) lang
  | _ :: "gram" :: "review" :: rest ->
      let lang, rest = extract_flag "--lang" rest in
      let n = match rest with n :: _ -> int_of_string_opt n | [] -> None in
      cmd_gram_review (default_grammar_path ()) n lang
  | _ :: "gram" :: "list" :: rest ->
      let lang, _rest = extract_flag "--lang" rest in
      cmd_gram_list (default_grammar_path ()) lang
  | _ :: "gram" :: "show" :: id :: _ -> cmd_gram_show (default_grammar_path ()) (parse_id_arg id)
  | _ :: "gram" :: "due" :: rest ->
      let lang, _rest = extract_flag "--lang" rest in
      cmd_gram_due (default_grammar_path ()) lang
  | _ :: ("help" | "-h" | "--help") :: _ -> usage ()
  | [ _ ] -> interactive_menu path None
  | _ ->
      print_endline "Unknown command.\n";
      usage ()

let main () =
  let path = default_deck_path () in
  try main_dispatch path
  with Stdin_closed ->
    print_newline ();
    exit 0
