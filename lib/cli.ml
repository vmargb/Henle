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

let status_color (status : Card.status) : string =
  match status with
  | Card.New -> "90" (* gray *)
  | Card.Drilling -> "33" (* yellow *)
  | Card.Fuzzy -> "35" (* magenta *)
  | Card.Intuitive -> "32" (* green *)
  | Card.Mastered -> "34" (* blue *)

let colored_status (status : Card.status) : string =
  colorize (status_color status) (Card.status_to_string status)

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
  Printf.printf "%s: %s\n" (dim "status") (colored_status c.status);
  Printf.printf "%s: %s\n" (dim "sentence") c.sentence;
  Printf.printf "%s: %s\n" (dim "translation") c.translation;
  (match c.notes with Some n -> Printf.printf "%s: %s\n" (dim "notes") n | None -> ());
  (match c.source with Some s -> Printf.printf "%s: %s\n" (dim "source") s | None -> ());
  Printf.printf "%s: %d/3\n" (dim "difficulty") c.difficulty;
  Printf.printf "%s: %d/3\n" (dim "importance") c.importance;
  Printf.printf "%s: %d\n" (dim "streak") c.streak;
  Printf.printf "%s: %d day(s)\n" (dim "interval") c.interval_days;
  (match c.last_review with
  | Some t -> Printf.printf "%s: %s\n" (dim "last review") (format_date t)
  | None -> Printf.printf "%s: never\n" (dim "last review"));
  Printf.printf "%s: %s\n" (dim "next review") (format_date c.next_review);
  (match Card.average_drill_reps c with
  | Some avg ->
      Printf.printf "%s: %d attempt(s), %.1f avg reps%s\n" (dim "drill history") c.drill_attempts avg
        (match c.last_drill_reps with Some n -> Printf.sprintf " (last: %d)" n | None -> "")
  | None -> Printf.printf "%s: not drilled yet\n" (dim "drill history"))

let list_row (c : Card.t) =
  let lang_col = colored_padded (language_color c.language) (Printf.sprintf "%-12s" (Card.truncate 12 c.language)) in
  let status_col = colored_padded (status_color c.status) (Printf.sprintf "%-10s" (Card.status_to_string c.status)) in
  Printf.printf "%-4d %s %-44s %s %1d/3  %1d/3  %5dd  %s\n" c.id lang_col
    (Card.truncate 44 c.sentence)
    status_col
    c.difficulty c.importance c.interval_days
    (format_date c.next_review)

let list_header () =
  Printf.printf "%s %s %s %s %s %s %s  %s\n"
    (bold "ID") (bold "LANG") (bold "SENTENCE")
    (bold "STATUS") (bold "DIFF") (bold "IMP") (bold "INTVL") (bold "NEXT REVIEW");
  print_rule ()

let status_legend () =
  print_endline
    (Printf.sprintf "  %s = not drilled yet   %s/%s = still working on it   \
     %s = clicked, in review rotation   %s = rarely reviewed"
       (colored_status Card.New) (colored_status Card.Drilling) (colored_status Card.Fuzzy)
       (colored_status Card.Intuitive) (colored_status Card.Mastered))

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
      print_string "\027[1A"; (* undo the newline the terminal echoed for Enter *)
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
    List.iteri
      (fun i (c : Card.t) ->
        print_rule ();
        Printf.printf "%s %d of %d\n" (bold "Card") (i + 1) total;
        Printf.printf "#%d  [%s]  (%s)\n" c.Card.id (colored_status c.Card.status) (colored_language c.Card.language);
        Printf.printf "  %s\n" c.Card.sentence;
        (match c.Card.notes with Some n -> Printf.printf "  notes: %s\n" n | None -> ());
        print_newline ();
        let skip = String.lowercase_ascii (prompt (info "Press Enter to drill this now, or type 's' to skip it: ")) = "s" in
        if not skip then begin
          let aha, reps = drill_loop 1 in
          let with_stats (c : Card.t) =
            {
              c with
              Card.drill_attempts = c.Card.drill_attempts + 1;
              total_drill_reps = c.Card.total_drill_reps + reps;
              last_drill_reps = Some reps;
            }
          in
          let updated =
            if aha then
              (* A quick click is a stronger signal than a long grind, so it
                 earns a slightly longer first interval before review. *)
              let initial_interval = if reps <= 3 then 2 else 1 in
              with_stats
                {
                  c with
                  Card.status = Card.Intuitive;
                  interval_days = initial_interval;
                  next_review = now () +. (float_of_int initial_interval *. Scheduler.day);
                }
            else
              with_stats
                { c with Card.status = (if c.Card.status = Card.New then Card.Drilling else Card.Fuzzy) }
          in
          deck := Deck.update !deck updated;
          Storage.save_deck path !deck;
          let avg_note =
            match Card.average_drill_reps updated with
            | Some avg when updated.Card.drill_attempts > 1 ->
                Printf.sprintf " (%d reps this time, %.1f avg over %d attempts)" reps avg
                  updated.Card.drill_attempts
            | _ -> Printf.sprintf " (%d rep%s)" reps (if reps = 1 then "" else "s")
          in
          print_endline
            ((if aha then success "-> nice, that's marked as Intuitive." else warning "-> no problem, it'll come back in your next drill session.")
            ^ avg_note)
        end;
        print_newline ())
      cards;
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
      Printf.printf "(%d more waiting -- run `henle review` again to keep going.)\n" (total_due - total);
    print_newline ();
    List.iteri
      (fun i (c : Card.t) ->
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
            let updated = Scheduler.schedule_review c rating t (Random.float 1.0) in
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
              (format_date updated.Card.next_review)
              (colored_status updated.Card.status);
            if updated.Card.status = Card.Intuitive && updated.Card.streak >= 5 then begin
              if prompt_yn (warning "  This one's felt easy for a while. Mark it Mastered (review it much less often)?") then begin
                let interval = Scheduler.max_interval_by_importance updated.Card.importance in
                let mastered =
                  { updated with Card.status = Card.Mastered; interval_days = interval; next_review = t +. (float_of_int interval *. Scheduler.day) }
                in
                deck := Deck.update !deck mastered;
                Storage.save_deck path !deck;
                print_endline (success "  -> marked Mastered.")
              end
            end;
            print_newline ())
      due;
    print_endline (success "Review session complete.")
  end

(* ---------- list / show / edit / master ---------- *)

let cmd_list path status_filter (lang_opt : string option) =
  let deck = Storage.load_deck path in
  let status_opt =
    match status_filter with
    | None -> None
    | Some s -> (
        match Card.status_of_string_opt s with
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
  let difficulty = prompt_int_default (info "Difficulty (0=easy, 3=hard)") c.Card.difficulty ~min:0 ~max:3 in
  let importance = prompt_int_default (info "Importance (0=low priority, 3=high)") c.Card.importance ~min:0 ~max:3 in
  let updated = { c with Card.language; sentence; translation; notes; source; difficulty; importance } in
  let deck = Deck.update deck updated in
  Storage.save_deck path deck;
  print_endline (success "Saved.")

let cmd_master path id =
  let deck = Storage.load_deck path in
  let c = get_card_or_fail deck id in
  let t = now () in
  let interval = Scheduler.max_interval_by_importance c.Card.importance in
  let updated =
    { c with Card.status = Card.Mastered; interval_days = interval; next_review = t +. (float_of_int interval *. Scheduler.day) }
  in
  let deck = Deck.update deck updated in
  Storage.save_deck path deck;
  Printf.printf "%s #%d marked Mastered, it'll be reviewed only rarely from now on.\n" (success "Card") id

let cmd_unmaster path id =
  let deck = Storage.load_deck path in
  let c = get_card_or_fail deck id in
  let t = now () in
  let updated = { c with Card.status = Card.Intuitive; interval_days = 14; next_review = t +. (14.0 *. Scheduler.day) } in
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
  Printf.printf "Deck file: %s  (override with $HENLE_DECK)\n" (bold (default_deck_path ()))

(* ---------- guided menu (default entry point) ---------- *)

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

let rec interactive_menu path (lang_filter : string option) =
  clear_screen ();
  let deck = Storage.load_deck path in
  let t = now () in
  let drill_n = List.length (Deck.filter_by_language (Deck.drillable deck) lang_filter) in
  let review_n = List.length (Deck.filter_by_language (Deck.due_for_review deck t) lang_filter) in
  print_newline ();
  print_endline (bold "Henle, sentence drilling & intuition-based review");
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
