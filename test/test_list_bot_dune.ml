open List_bot_dune.Telelisp

let _ =
  let run_example s =
    let ctx = [] in
    let pctx = [] in
    let string_of_res g v =
      match v with
      | Ok(x) -> Printf.sprintf "Ok(%s)" (g x)
      | Err(s) -> Printf.sprintf "Err(%s)" s
    in
    let simple_res x =
      match x with
      | Ok((a, _, _)) -> Ok(a)
      | Err(s) -> Err(s)
    in
    let res = eval ctx pctx s in
    print_endline @@ string_of_res (fun (a, _, _) -> string_of_sexp a) res;
    simple_res res
  in
  let nil = Nil in
  let _a a = Atom(a) in
  let _i i = Int(i) in
  let cons a b = Cons(a, b) in
  let lambda = Atom("lambda") in
  let example1 =
    cons (_a "+") (cons (_i 1) (cons (_i 2) (cons (_i 3) nil)))
  in
  let example2 =
    cons (_a "-") (cons (_i 1) (cons (_i 2) (cons (_i 3) nil)))
  in
  let example3 =
    let sqr = 
      (cons 
        lambda 
        (cons 
          (cons (_a "x") nil) 
          (cons 
            (cons 
              (_a "*") 
              (cons 
                (_a "x") 
                (cons 
                  (_a "x")
                  nil))) 
            nil))) 
    in
    cons (_a "apply") (cons sqr (cons (_i 3) nil))
  in
  assert (run_example example1 = Ok(Int(6)));
  assert (run_example example2 = Ok(Int(-4)));
  assert (run_example example3 = Ok(Int(9)))