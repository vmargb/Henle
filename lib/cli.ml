(* Interactive command-line interface for Henle *)

let now () = Unix.time ()

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
let wait_for_continue () = ignore (prompt "\nPress Enter to return to the menu: ")

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

(* ---------- card display ---------- *)

let show_card_full (c : Card.t) =
  Printf.printf "id:             %d\n" c.id;
  Printf.printf "status:         %s\n" (Card.status_to_string c.status);
  Printf.printf "sentence:       %s\n" c.sentence;
  Printf.printf "translation:    %s\n" c.translation;
  (match c.notes with Some n -> Printf.printf "notes:          %s\n" n | None -> ());
  (match c.source with Some s -> Printf.printf "source:         %s\n" s | None -> ());
  Printf.printf "difficulty:     %d/3\n" c.difficulty;
  Printf.printf "importance:     %d/3\n" c.importance;
  Printf.printf "streak:         %d\n" c.streak;
  Printf.printf "interval:       %d day(s)\n" c.interval_days;
  (match c.last_review with
  | Some t -> Printf.printf "last review:    %s\n" (format_date t)
  | None -> Printf.printf "last review:    never\n");
  Printf.printf "next review:    %s\n" (format_date c.next_review)

let list_row (c : Card.t) =
  Printf.printf "%-4d %-56s %-10s %1d/3  %1d/3  %5dd  %s\n" c.id
    (Card.truncate 56 c.sentence)
    (Card.status_to_string c.status)
    c.difficulty c.importance c.interval_days
    (format_date c.next_review)

let list_header () =
  Printf.printf "%-4s %-56s %-10s %-5s %-5s %6s  %s\n" "ID" "SENTENCE"
    "STATUS" "DIFF" "IMP" "INTVL" "NEXT REVIEW";
  print_rule ()

let status_legend () =
  print_endline
    "  New = not drilled yet   Drilling/Fuzzy = still working on it   \
     Intuitive = clicked, in review rotation   Mastered = rarely reviewed"

(* ---------- add ---------- *)

let prompt_one_sentence () =
  let sentence =
    prompt "Sentence (in the language you're learning), or leave blank to stop: "
  in
  if sentence = "" then None
  else begin
    let translation = prompt "  Your translation of it: " in
    let notes = prompt_opt "  Any grammar or usage notes? (optional, Enter to skip): " in
    let source = prompt_opt "  Where's it from? (optional, Enter to skip): " in
    Some (sentence, translation, notes, source)
  end

(* Adds one or more sentences interactively. Returns the ids that were
   added, so the menu, or this function itself can offer to
   drill that batch together right away. *)
let cmd_add path : int list =
  let is_first_run = not (Sys.file_exists path) in
  if is_first_run then begin
    print_endline "Welcome to Henle.";
    print_endline "";
    print_endline "The idea: you don't memorize grammar, you drill them";
    print_endline "read one over and over until the meaning just lands, with no";
    print_endline "English in your head. Once it clicks, spaced review keeps";
    print_endline "that feeling fresh over time. Let's add your first sentence.";
    print_newline ()
  end
  else begin
    print_endline "Adding sentence(s). Add as many as you like, one at a time";
    print_endline "they'll be offered as a single drill session once you're done.";
    print_newline ()
  end;
  let deck = ref (Storage.load_deck path) in
  let added_ids = ref [] in
  let rec loop () =
    match prompt_one_sentence () with
    | None -> ()
    | Some (sentence, translation, notes, source) ->
        let deck', card =
          Deck.add_card !deck ~sentence ~translation ~notes ~source ~now:(now ())
        in
        deck := deck';
        Storage.save_deck path !deck;
        added_ids := card.Card.id :: !added_ids;
        Printf.printf "  -> saved as #%d.\n\n" card.Card.id;
        loop ()
  in
  loop ();
  List.rev !added_ids

(* ---------- drilling ---------- *)

let drill_intro n =
  Printf.printf "%d sentence(s) to drill.\n" n;
  print_endline "This isn't a test, there's no wrong answer. For each one, read or";
  print_endline "say it aloud, repeatedly, until the meaning lands directly and you";
  print_endline "stop mentally translating. Then say whether you felt that click.";
  print_newline ()

let run_drill_on path (cards : Card.t list) =
  if cards = [] then
    print_endline "Nothing to drill right now, every sentence is either intuitive or mastered."
  else begin
    let deck = ref (Storage.load_deck path) in
    drill_intro (List.length cards);
    List.iter
      (fun (c : Card.t) ->
        print_rule ();
        Printf.printf "#%d  [%s]\n" c.Card.id (Card.status_to_string c.Card.status);
        Printf.printf "  %s\n" c.Card.sentence;
        (match c.Card.notes with Some n -> Printf.printf "  notes: %s\n" n | None -> ());
        print_newline ();
        let skip = String.lowercase_ascii (prompt "Press Enter to drill this now, or type 's' to skip it: ") = "s" in
        if not skip then begin
          ignore (prompt "Read/repeat it as many times as you need, then press Enter: ");
          let aha = prompt_yn "Did it click, did you feel it directly, without translating?" in
          let updated =
            if aha then
              {
                c with
                Card.status = Card.Intuitive;
                interval_days = 1;
                next_review = now () +. Scheduler.day;
              }
            else
              { c with Card.status = (if c.Card.status = Card.New then Card.Drilling else Card.Fuzzy) }
          in
          deck := Deck.update !deck updated;
          Storage.save_deck path !deck;
          print_endline
            (if aha then "-> nice, that's marked as Intuitive. It'll come up for review tomorrow."
             else "-> no problem, it'll come back in your next drill session.")
        end;
        print_newline ())
      cards;
    print_endline "Drilling session complete."
  end

let cmd_drill path limit =
  let deck = Storage.load_deck path in
  let candidates = Deck.sort_by_due (Deck.drillable deck) in
  let candidates =
    match limit with
    | None -> candidates
    | Some n -> List.filteri (fun i _ -> i < n) candidates
  in
  run_drill_on path candidates

(* ---------- review ---------- *)

let cmd_review path =
  let deck = ref (Storage.load_deck path) in
  let t = now () in
  let due = Deck.sort_by_due (Deck.due_for_review !deck t) in
  if due = [] then print_endline "No sentences due for review right now."
  else begin
    Printf.printf "%d sentence(s) due.\n" (List.length due);
    print_endline "This is a quick check-in, not a test of memory: for each sentence,";
    print_endline "rate how direct and intuitive it feels *right now*. 'Hard' just";
    print_endline "means it needs more drilling again, it isn't a failure.";
    print_newline ();
    List.iter
      (fun (c : Card.t) ->
        print_rule ();
        Printf.printf "#%d\n" c.Card.id;
        Printf.printf "  %s\n" c.Card.sentence;
        print_newline ();
        let rec ask () =
          match
            prompt
              "How does it feel? (e = Easy/direct, g = Good/mostly direct, h = Hard/still translating, s = skip): "
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
            Printf.printf "  translation: %s\n" c.Card.translation;
            let updated = Scheduler.schedule_review c rating t in
            deck := Deck.update !deck updated;
            Storage.save_deck path !deck;
            Printf.printf "  -> %s. Next review: %s. (status: %s)\n"
              (Scheduler.rating_to_string rating)
              (format_date updated.Card.next_review)
              (Card.status_to_string updated.Card.status);
            if updated.Card.status = Card.Intuitive && updated.Card.streak >= 5 then begin
              if prompt_yn "  This one's felt easy for a while. Mark it Mastered (review it much less often)?" then begin
                let interval = Scheduler.max_interval_by_importance updated.Card.importance in
                let mastered =
                  { updated with Card.status = Card.Mastered; interval_days = interval; next_review = t +. (float_of_int interval *. Scheduler.day) }
                in
                deck := Deck.update !deck mastered;
                Storage.save_deck path !deck;
                print_endline "  -> marked Mastered."
              end
            end;
            print_newline ())
      due;
    print_endline "Review session complete."
  end

(* ---------- list / show / edit / master ---------- *)

let cmd_list path status_filter =
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
  let cards = Deck.sort_by_id (Deck.by_status deck status_opt) in
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
  print_endline "Editing. Press Enter to keep the current value, or '-' to clear an optional field.";
  let sentence = prompt_default "Sentence" c.Card.sentence in
  let translation = prompt_default "Translation" c.Card.translation in
  let notes = prompt_opt_default "Notes" c.Card.notes in
  let source = prompt_opt_default "Source" c.Card.source in
  let difficulty = prompt_int_default "Difficulty (0=easy, 3=hard)" c.Card.difficulty ~min:0 ~max:3 in
  let importance = prompt_int_default "Importance (0=low priority, 3=high)" c.Card.importance ~min:0 ~max:3 in
  let updated = { c with Card.sentence; translation; notes; source; difficulty; importance } in
  let deck = Deck.update deck updated in
  Storage.save_deck path deck;
  print_endline "Saved."

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
  Printf.printf "Card #%d marked Mastered, it'll be reviewed only rarely from now on.\n" id

let cmd_unmaster path id =
  let deck = Storage.load_deck path in
  let c = get_card_or_fail deck id in
  let t = now () in
  let updated = { c with Card.status = Card.Intuitive; interval_days = 14; next_review = t +. (14.0 *. Scheduler.day) } in
  let deck = Deck.update deck updated in
  Storage.save_deck path deck;
  Printf.printf "Card #%d is back in normal rotation (next review in 14 days).\n" id

let cmd_due path =
  let deck = Storage.load_deck path in
  let t = now () in
  let drill_n = List.length (Deck.drillable deck) in
  let review_n = List.length (Deck.due_for_review deck t) in
  Printf.printf "%d ready to drill, %d due for review.\n" drill_n review_n

let usage () =
  print_endline "henle, Henle-style sentence drilling with intuition-based SRS";
  print_endline "";
  print_endline "Run `henle` with no arguments for a guided menu. Or use these";
  print_endline "commands directly:";
  print_endline "";
  print_endline "  henle add                     add sentence(s) to the deck";
  print_endline "  henle drill [N]               drilling session: repeat sentences until they click (default: 5)";
  print_endline "  henle review                  review session: rate how intuitive due sentences feel";
  print_endline "  henle list [--status STATUS]  list sentences (new/drilling/fuzzy/intuitive/mastered)";
  print_endline "  henle show <id>               show full details for a sentence";
  print_endline "  henle edit <id>               edit a sentence's fields";
  print_endline "  henle master <id>             suspend a sentence from normal rotation (rarely reviewed)";
  print_endline "  henle unmaster <id>           bring a Mastered sentence back into rotation";
  print_endline "  henle due                     show counts of what's ready to drill/review";
  print_endline "  henle help                    show this message";
  print_endline "";
  print_endline "Drilling vs. review, in short:";
  print_endline "  drill  = for NEW or still-fuzzy sentences: repeat until it clicks.";
  print_endline "  review = for sentences that already clicked: quick check that the feeling stuck.";
  print_endline "";
  print_endline ("Deck file: " ^ default_deck_path () ^ "  (override with $HENLE_DECK)")

(* ---------- guided menu (default entry point) ---------- *)

let rec interactive_menu path =
  clear_screen ();
  let deck = Storage.load_deck path in
  let t = now () in
  let drill_n = List.length (Deck.drillable deck) in
  let review_n = List.length (Deck.due_for_review deck t) in
  print_newline ();
  print_endline "Henle, sentence drilling & intuition-based review";
  print_newline ();
  Printf.printf "  %d ready to drill   (still building intuition, repeat until it clicks)\n" drill_n;
  Printf.printf "  %d due for review   (already clicked, check the feeling has stuck)\n" review_n;
  print_newline ();
  print_endline "What would you like to do?";
  print_endline "  1) Add sentence(s)";
  print_endline "  2) Drill   : repeat new sentences until they click";
  print_endline "  3) Review  : review old sentences that already clicked";
  print_endline "  4) List all sentences";
  print_endline "  5) Full command reference (for scripting/power use)";
  print_endline "  q) Quit";
  match String.lowercase_ascii (prompt "> ") with
  | "1" | "add" ->
      clear_screen ();
      let ids = cmd_add path in
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
      interactive_menu path
  | "2" | "drill" ->
      clear_screen ();
      cmd_drill path (Some 5);
      wait_for_continue ();
      interactive_menu path
  | "3" | "review" ->
      clear_screen ();
      cmd_review path;
      wait_for_continue ();
      interactive_menu path
  | "4" | "list" ->
      clear_screen ();
      cmd_list path None;
      wait_for_continue ();
      interactive_menu path
  | "5" | "help" ->
      clear_screen ();
      usage ();
      wait_for_continue ();
      interactive_menu path
  | "q" | "quit" | "exit" -> ()
  | _ ->
      print_endline "Not a valid option, pick a number from the list, or q to quit.";
      interactive_menu path

let main_dispatch path =
  clear_screen ();
  match Array.to_list Sys.argv with
  | _ :: "add" :: _ -> ignore (cmd_add path)
  | _ :: "drill" :: rest -> (
      match rest with
      | n :: _ -> (
          match int_of_string_opt n with
          | Some n -> cmd_drill path (Some n)
          | None -> cmd_drill path (Some 5))
      | [] -> cmd_drill path (Some 5))
  | _ :: "review" :: _ -> cmd_review path
  | _ :: "list" :: "--status" :: s :: _ -> cmd_list path (Some s)
  | _ :: "list" :: _ -> cmd_list path None
  | _ :: "show" :: id :: _ -> cmd_show path (parse_id_arg id)
  | _ :: "edit" :: id :: _ -> cmd_edit path (parse_id_arg id)
  | _ :: "master" :: id :: _ -> cmd_master path (parse_id_arg id)
  | _ :: "unmaster" :: id :: _ -> cmd_unmaster path (parse_id_arg id)
  | _ :: "due" :: _ -> cmd_due path
  | _ :: ("help" | "-h" | "--help") :: _ -> usage ()
  | [ _ ] -> interactive_menu path
  | _ ->
      print_endline "Unknown command.\n";
      usage ()

let main () =
  let path = default_deck_path () in
  try main_dispatch path
  with Stdin_closed ->
    print_newline ();
    exit 0
