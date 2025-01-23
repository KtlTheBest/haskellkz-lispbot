(* What do I need to have a minimal lisp evaluator? *)

type body =
  | Nil
  | Cons of body * body
  | Atom of string
  | String of string
  | Int of int
  | Char of char
  | Bool of bool

type 'a res =
  | Ok of 'a
  | Err of string

let ok x = Ok(x)
let err s = Err(s)

let string_of_sexp s =
  let rec loop s =
    match s with
    | Nil -> "NIL"
    | Atom(s) -> s
    | String(s) -> Printf.sprintf "\"%s\"" s
    | Int(s) -> string_of_int s
    | Char(c) -> Printf.sprintf "#\\%c" c
    | Bool(true) -> "true"
    | Bool(false) -> "false"
    | Cons(Cons(a, b), Nil) ->
      Printf.sprintf "(%s)" @@ loop (Cons(a, b))
    | Cons(Cons(a, b), (Atom(_) as x'))
    | Cons(Cons(a, b), (String(_) as x'))
    | Cons(Cons(a, b), (Int(_) as x'))
    | Cons(Cons(a, b), (Char(_) as x'))
    | Cons(Cons(a, b), (Bool(_) as x')) ->
      Printf.sprintf "(%s) . %s" 
        (loop (Cons(a, b))) 
        (loop x')
    | Cons(Cons(a, b), rest) ->
      Printf.sprintf "(%s) %s" 
        (loop (Cons(a, b))) 
        (loop rest)
    | Cons(x, Nil) -> loop x
    | Cons(x, (Atom(_) as y))
    | Cons(x, (String(_) as y))
    | Cons(x, (Int(_) as y))
    | Cons(x, (Char(_) as y))
    | Cons(x, (Bool(_) as y)) ->
      Printf.sprintf "%s . %s" 
        (loop x) 
        (loop y)
    | Cons(x, rest) ->
      Printf.sprintf "%s %s"
        (loop x)
        (loop rest)
  in
  match s with
  | Cons(_, _) -> Printf.sprintf "(%s)" (loop s)
  | _ -> loop s

let rec eval ctx pctx s =
  let ret a b c = ok (a, b, c) in
  (*
  let (>>=) x f =
    match x with
    | Ok((a, b, c)) -> f b c a
    | Err(s) -> Err(s)
  in
  *)
  match s with
  | Nil
  | Int(_)
  | Char(_)
  | Bool(_)
  | String(_) -> ret s ctx pctx
  | Cons(Atom(cmd), rest) ->
    (match cmd with
    | "block" | "BLOCK" ->
      let rec loop ctx pctx last_val l =
        match l with
        | Nil -> ret last_val ctx pctx
        | Cons(command, rest) ->
          let res = eval ctx pctx command in
          (match res with
          | Ok((new_val, new_ctx, new_pctx)) -> loop new_ctx new_pctx new_val rest
          | Err(s) -> Err(s))
        | _ -> err (Printf.sprintf "Error: unhandled structure in block evaluation: %s" (string_of_sexp l))
      in
      let res = loop ctx pctx Nil rest in
      (match res with
      | Ok(v, _, new_pctx) -> ret v ctx new_pctx
      | Err(s) -> Err(s))
    | "car" | "CAR" ->
      (match rest with
      | Cons(x, _) -> ret x ctx pctx
      | _ -> err (Printf.sprintf "Error: couldn't get a car of %s" (string_of_sexp rest)))
    | "cdr" | "CDR" ->
      (match rest with
      | Cons(_, y) -> ret y ctx pctx
      | _ -> err (Printf.sprintf "Error: couldn't get a cdr of %s" (string_of_sexp rest)))
    | "cons" | "CONS" ->
      (match rest with
      | Cons(_, _) -> ret rest ctx pctx
      | _ -> err (Printf.sprintf "Error: couldn't make a cons of %s" (string_of_sexp rest)))
    | "quote" | "QUOTE" -> ret rest ctx pctx
    | "list" | "LIST" -> eval ctx pctx rest
    | "+" ->
      let rec loop ans l =
        match l with
        | Nil -> ret (Int(ans)) ctx pctx
        | Int(v) -> ret (Int(ans + v)) ctx pctx
        | Atom(name) ->
          (match List.assoc_opt name (ctx @ pctx) with
          | Some(Int(v)) -> ret (Int(ans + v)) ctx pctx
          | Some(x) -> err (Printf.sprintf "Error: tried to perform addition with %s = %s, expected Int" name (string_of_sexp x))
          | None -> err (Printf.sprintf "Error: %s is not defined in the context!" name))
        | Cons(Int(a), rest) -> loop (ans + a) rest
        | Cons(Atom(name), rest) ->
          (match List.assoc_opt name (ctx @ pctx) with
          | Some(Int(a)) -> loop (ans + a) rest
          | Some(x) -> err (Printf.sprintf "Error: tried to perform addition with %s = %s, expected Int" name (string_of_sexp x))
          | None -> err (Printf.sprintf "Error, %s is not defined in the context!" name))
        | Cons(x, _) -> err (Printf.sprintf "Error: tried to perform addition with %s, expected Int" (string_of_sexp x))
        | x -> err (Printf.sprintf "Error: tried to perform addition with %s, expected Int" (string_of_sexp x))
      in
      loop 0 rest
    | "-" ->
      let rec loop ans l =
        match l with
        | Nil -> ret (Int(ans)) ctx pctx
        | Int(v) -> ret (Int(ans - v)) ctx pctx
        | Cons(Int(a), rest) -> loop (ans - a) rest
        | Cons(Atom(name), rest) ->
          (match List.assoc_opt name (ctx @ pctx) with
          | Some(Int(a)) -> loop (ans - a) rest
          | Some(x) -> err (Printf.sprintf "Error: tried to perform addition with %s = %s, expected Int" name (string_of_sexp x))
          | None -> err (Printf.sprintf "Error, %s is not defined in the context!" name))
        | Cons(x, _) -> err (Printf.sprintf "Error: tried to perform addition with %s, expected Int" (string_of_sexp x))
        | x -> err (Printf.sprintf "Error: tried to perform subtraction with %s, expected Int" (string_of_sexp x))
      in
      (match rest with
      | Nil -> ret (Int(0)) ctx pctx
      | Int(_) -> ret rest ctx pctx
      | Atom(name) ->
        (match List.assoc_opt name (ctx @ pctx) with
        | Some(Int(v)) -> ret (Int(v)) ctx pctx
        | Some(x) -> err (Printf.sprintf "Error: tried to perform subtraction with %s = %s, expected Int" name (string_of_sexp x))
        | None -> err (Printf.sprintf "Error: %s is not defined in the context!" name))
      | Cons(Int(v), rest') -> loop v rest'
      | Cons(Atom(name), rest') ->
        (match List.assoc_opt name (ctx @ pctx) with
        | Some(Int(v)) -> loop v rest'
        | Some(x) -> err (Printf.sprintf "Error: tried to perform subtraction with %s = %s, expected Int" name (string_of_sexp x))
        | None -> err (Printf.sprintf "Error: %s is not defined in the context!" name))
      | Cons(x, _) -> err (Printf.sprintf "Error: tried to perform subtraction with %s, expected Int" (string_of_sexp x))
      | _ -> err (Printf.sprintf "Error: tried to perform subtraction with %s, expected Int" (string_of_sexp rest)))
    | "*" ->
      let rec loop ans l =
        match l with
        | Nil -> ret (Int(ans)) ctx pctx
        | Int(v) -> ret (Int(ans * v)) ctx pctx
        | Atom(name) ->
          (match List.assoc_opt name (ctx @ pctx) with
          | Some(Int(v)) -> ret (Int(ans + v)) ctx pctx
          | Some(x) -> err (Printf.sprintf "Error: tried to perform multiplication with %s = %s, expected Int" name (string_of_sexp x))
          | None -> err (Printf.sprintf "Error: %s is not defined in the context!" name))
        | Cons(Int(a), rest) -> loop (ans + a) rest
        | Cons(Atom(name), rest) ->
          (match List.assoc_opt name (ctx @ pctx) with
          | Some(Int(a)) -> loop (ans * a) rest
          | Some(x) -> err (Printf.sprintf "Error: tried to perform multiplication with %s = %s, expected Int" name (string_of_sexp x))
          | None -> err (Printf.sprintf "Error, %s is not defined in the context!" name))
        | Cons(x, _) -> err (Printf.sprintf "Error: tried to perform multiplication with %s, expected Int" (string_of_sexp x))
        | x -> err (Printf.sprintf "Error: tried to perform multiplication with %s, expected Int" (string_of_sexp x))
      in
      loop 1 rest
    | "apply" | "APPLY" -> 
      let list_len l =
        let rec loop ans l =
          match l with
          | Nil -> ans
          | Cons(_, rest) -> loop (ans + 1) rest
          | _ -> 1
        in
        loop 0 l
      in
      let check_lambda argnames args body =
        let rec ensure_argnames_are_atoms l =
          match l with
          | Nil -> true
          | Cons(Atom(_), rest) -> ensure_argnames_are_atoms rest
          | _ -> false
        in
        (match ensure_argnames_are_atoms argnames with
        | true ->
          let module S = Set.Make(String) in
          let ensure_unique_names l =
            let rec collect_atoms s l =
              match l with
              | Nil -> s
              | Cons(Atom(name), rest) ->
                collect_atoms (S.add name s) rest
              | _ -> failwith "eval apply: random crash"
            in
            let s = collect_atoms (S.empty) l in
            S.cardinal s = list_len l
          in
          (match ensure_unique_names argnames with
          | true ->
            let ensure_args_count_match args vals =
              list_len args = list_len vals
            in
            (match ensure_args_count_match argnames args with
            | true ->
              let bind_args_to_names argnames values =
                let rec loop ans a b =
                  match a, b with
                  | Nil, Nil -> ans
                  | Cons(Atom(name), rest_a), Cons(v, rest_b) ->
                    loop (ans @ [(name, v)]) rest_a rest_b
                  | _, _ -> failwith @@ Printf.sprintf "eval apply: something went wrong when binding vars: %s and %s" (string_of_sexp a) (string_of_sexp b)
                in
                loop [] argnames values
              in
              let bound_values = bind_args_to_names argnames args in
              let new_ctx = bound_values @ ctx in
              eval new_ctx pctx body
            | false -> err (Printf.sprintf "Error: the number of expected arguments and provided arguments don't match: %s and %s"
                                            (string_of_sexp argnames)
                                            (string_of_sexp args))
            )
          | false -> err (Printf.sprintf "Error: some of the argnames are not unique!"))
        | false -> err (Printf.sprintf "Error: some of the argnames are not atoms! %s" (string_of_sexp argnames)))
      in
      (match rest with
      | Nil -> err (Printf.sprintf "Error: expected a function and an arg-list to 'apply', received nothing!")
      | Cons(_, Nil) -> err (Printf.sprintf "Error: expected an arg-list to 'apply', received only function!")
      | Cons(func, args) -> 
        let func' = eval ctx pctx func in
        (match func' with
        | Ok((func', ctx', new_pctx)) ->
          (match func' with
          | Atom(f_name) -> 
            let is_builtin = function
              | "+"
              | "-"
              | "*"
              | "quote" | "QUOTE"
              | "block" | "BLOCK"
              | "lambda" | "LAMBDA" 
              | "apply" | "APPLY"
              | "car" | "CAR"
              | "cdr" | "CDR"
              | "cons" | "CONS" -> true
              | _ -> false
            in
            (match is_builtin f_name with
            | true -> eval ctx pctx (Cons(Atom(f_name), args))
            | false ->
              (match List.assoc_opt f_name (ctx' @ new_pctx) with
              | Some(Cons(argnames, Cons(body, Nil))) -> check_lambda argnames args body
              | Some(t) -> err (Printf.sprintf "Error: failed to apply as a function: %s" (string_of_sexp t))
              | None -> err (Printf.sprintf "Error: %s is not defined in the context!" f_name)))
          | Cons(argnames, Cons(body, Nil)) -> check_lambda argnames args body
          | _ -> err (Printf.sprintf "Error: couldn't apply %s" (string_of_sexp func)))
        | Err(s) -> Err(s)
        )
      | _ -> err (Printf.sprintf "Error: Couldn't perform an 'apply' of %s" (string_of_sexp rest)))
    | "lambda" | "LAMBDA" -> 
      (match rest with
      | Cons(arg_names, Cons(body, Nil)) ->
        ret (Cons(arg_names, Cons(body, Nil))) ctx pctx
      | _ -> err (Printf.sprintf "Error: couldn't construct a lambda of %s" (string_of_sexp rest)))
    | _ ->
      (match List.assoc_opt cmd ctx with
      | Some(x) -> ret x ctx pctx
      | None -> err (Printf.sprintf "Error: %s is not defined in the context!" cmd)
      )
    )
  | _ -> err (Printf.sprintf "Error: tried to evaluate something weird: %s" (string_of_sexp s))
