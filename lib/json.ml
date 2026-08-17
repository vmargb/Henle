type t =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of t list
  | Assoc of (string * t) list

exception Parse_error of string

(* ---------- Printing ---------- *)

let escape_string s =
  let buf = Buffer.create (String.length s + 2) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let rec to_string ?(indent = 0) (j : t) : string =
  let pad n = String.make (n * 2) ' ' in
  match j with
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | Number f ->
      if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f
      else Printf.sprintf "%g" f
  | String s -> "\"" ^ escape_string s ^ "\""
  | List [] -> "[]"
  | List items ->
      let inner =
        List.map
          (fun i -> pad (indent + 1) ^ to_string ~indent:(indent + 1) i)
          items
      in
      "[\n" ^ String.concat ",\n" inner ^ "\n" ^ pad indent ^ "]"
  | Assoc [] -> "{}"
  | Assoc kvs ->
      let inner =
        List.map
          (fun (k, v) ->
            pad (indent + 1) ^ "\"" ^ escape_string k ^ "\": "
            ^ to_string ~indent:(indent + 1) v)
          kvs
      in
      "{\n" ^ String.concat ",\n" inner ^ "\n" ^ pad indent ^ "}"

(* ---------- Parsing ---------- *)

let parse (s : string) : t =
  let n = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < n then Some s.[!pos] else None in
  let advance () = incr pos in
  let error msg = raise (Parse_error (Printf.sprintf "%s at position %d" msg !pos)) in
  let rec skip_ws () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') -> advance (); skip_ws ()
    | _ -> ()
  in
  let expect c =
    match peek () with
    | Some c' when c' = c -> advance ()
    | _ -> error (Printf.sprintf "expected '%c'" c)
  in
  let parse_string () =
    expect '"';
    let buf = Buffer.create 16 in
    let rec loop () =
      match peek () with
      | None -> error "unterminated string"
      | Some '"' -> advance ()
      | Some '\\' -> (
          advance ();
          (match peek () with
          | Some '"' -> Buffer.add_char buf '"'; advance ()
          | Some '\\' -> Buffer.add_char buf '\\'; advance ()
          | Some '/' -> Buffer.add_char buf '/'; advance ()
          | Some 'n' -> Buffer.add_char buf '\n'; advance ()
          | Some 't' -> Buffer.add_char buf '\t'; advance ()
          | Some 'r' -> Buffer.add_char buf '\r'; advance ()
          | Some 'b' -> Buffer.add_char buf '\b'; advance ()
          | Some 'f' -> Buffer.add_char buf '\012'; advance ()
          | Some 'u' ->
              advance ();
              if !pos + 4 > n then error "bad unicode escape";
              let hex = String.sub s !pos 4 in
              pos := !pos + 4;
              let code = int_of_string ("0x" ^ hex) in
              (* Basic UTF-8 encoding *)
              if code < 0x80 then Buffer.add_char buf (Char.chr code)
              else if code < 0x800 then (
                Buffer.add_char buf (Char.chr (0xC0 lor (code lsr 6)));
                Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F))))
              else (
                Buffer.add_char buf (Char.chr (0xE0 lor (code lsr 12)));
                Buffer.add_char buf
                  (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
                Buffer.add_char buf (Char.chr (0x80 lor (code land 0x3F))))
          | _ -> error "bad escape sequence");
          loop ())
      | Some c -> Buffer.add_char buf c; advance (); loop ()
    in
    loop ();
    Buffer.contents buf
  in
  let is_digit c = c >= '0' && c <= '9' in
  let rec parse_value () =
    skip_ws ();
    match peek () with
    | Some '"' -> String (parse_string ())
    | Some '{' -> parse_object ()
    | Some '[' -> parse_array ()
    | Some 't' ->
        if !pos + 4 <= n && String.sub s !pos 4 = "true" then (
          pos := !pos + 4;
          Bool true)
        else error "expected 'true'"
    | Some 'f' ->
        if !pos + 5 <= n && String.sub s !pos 5 = "false" then (
          pos := !pos + 5;
          Bool false)
        else error "expected 'false'"
    | Some 'n' ->
        if !pos + 4 <= n && String.sub s !pos 4 = "null" then (
          pos := !pos + 4;
          Null)
        else error "expected 'null'"
    | Some c when c = '-' || is_digit c -> parse_number ()
    | _ -> error "unexpected character"
  and parse_number () =
    let start = !pos in
    if peek () = Some '-' then advance ();
    while match peek () with Some c when is_digit c -> true | _ -> false do
      advance ()
    done;
    if peek () = Some '.' then (
      advance ();
      while match peek () with Some c when is_digit c -> true | _ -> false do
        advance ()
      done);
    (match peek () with
    | Some ('e' | 'E') ->
        advance ();
        (match peek () with Some ('+' | '-') -> advance () | _ -> ());
        while match peek () with Some c when is_digit c -> true | _ -> false do
          advance ()
        done
    | _ -> ());
    Number (float_of_string (String.sub s start (!pos - start)))
  and parse_object () =
    expect '{';
    skip_ws ();
    if peek () = Some '}' then (advance (); Assoc [])
    else
      let rec loop acc =
        skip_ws ();
        let k = parse_string () in
        skip_ws ();
        expect ':';
        skip_ws ();
        let v = parse_value () in
        skip_ws ();
        match peek () with
        | Some ',' -> advance (); loop ((k, v) :: acc)
        | Some '}' -> advance (); Assoc (List.rev ((k, v) :: acc))
        | _ -> error "expected ',' or '}'"
      in
      loop []
  and parse_array () =
    expect '[';
    skip_ws ();
    if peek () = Some ']' then (advance (); List [])
    else
      let rec loop acc =
        let v = parse_value () in
        skip_ws ();
        match peek () with
        | Some ',' -> advance (); skip_ws (); loop (v :: acc)
        | Some ']' -> advance (); List (List.rev (v :: acc))
        | _ -> error "expected ',' or ']'"
      in
      loop []
  in
  let v = parse_value () in
  skip_ws ();
  v

(* ---------- Accessors ---------- *)

let member key j =
  match j with
  | Assoc kvs -> ( try List.assoc key kvs with Not_found -> Null)
  | _ -> Null

let to_str = function
  | String s -> s
  | _ -> raise (Parse_error "expected string")

let to_str_opt = function
  | Null -> None
  | String s -> Some s
  | _ -> raise (Parse_error "expected string or null")

let to_float = function
  | Number f -> f
  | _ -> raise (Parse_error "expected number")

let to_float_opt = function
  | Null -> None
  | Number f -> Some f
  | _ -> raise (Parse_error "expected number or null")

let to_int j = int_of_float (to_float j)
let to_int_opt j = Option.map int_of_float (to_float_opt j)
let to_list = function List l -> l | _ -> raise (Parse_error "expected list")
