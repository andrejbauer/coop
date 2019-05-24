type environment = Value.t list

type error =
  | InvalidDeBruijn of int
  | UnhandledOperation of Name.t
  | UnhandledSignal of Name.t
  | UnknownExternal of string
  | IllegalRenaming of Name.t
  | FunctionExpected
  | ComodelExpected
  | ComodelDoubleOperation of Name.t
  | PairExpected
  | PatternMismatch

exception Error of error Location.located

let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

let print_error err ppf =
  match err with

  | InvalidDeBruijn i ->
     Format.fprintf ppf "invalid de Bruijn index %d, please report" i

  | UnhandledOperation op ->
     Format.fprintf ppf "unhandled operation %t" (Name.print op)

  | UnhandledSignal sgl ->
     Format.fprintf ppf "terminated by signal %t" (Name.print sgl)

  | UnknownExternal s ->
     Format.fprintf ppf "unknown external %s" s

  | IllegalRenaming op ->
     Format.fprintf ppf "illegal comodel renaming %t, please report" (Name.print op)

  | FunctionExpected ->
     Format.fprintf ppf "function expected, please report"

  | ComodelExpected ->
     Format.fprintf ppf "comodel expected, please report"

  | ComodelDoubleOperation op ->
     Format.fprintf ppf "cannot combine models that both contain the coperation %t" (Name.print op)

  | PairExpected ->
     Format.fprintf ppf "pair expected, please report"

  | PatternMismatch ->
     Format.fprintf ppf "pattern mismatch"

let initial = []

let extend v env = v :: env

let rec extends vs env =
  match vs with
  | [] -> env
  | v :: vs ->
     let env = extend v env in
     extends vs env

let lookup ~loc i env =
  try
    List.nth env i
  with
  | Failure _ -> error ~loc (InvalidDeBruijn i)

let match_pattern p v =
  let rec fold us p v =
    match p, v with

    | Syntax.PattAnonymous, _ -> Some us

    | Syntax.PattVar, _ ->
       Some (v :: us)

    | Syntax.PattNumeral m, Value.Numeral n ->
       if m = n then Some us else None

    | Syntax.PattBoolean b, Value.Boolean b' ->
       if b = b' then Some us else None

    | Syntax.PattConstructor (cnstr, popt), Value.Constructor (cnstr', vopt) ->
       begin
         if not (Name.equal cnstr cnstr') then
           None
         else
           match popt, vopt with
           | None, None -> Some us
           | Some p, Some v -> fold us p v
           | Some _, None | None, Some _ -> None
       end

    | Syntax.PattTuple ps, Value.Tuple vs ->
       fold_tuple us ps vs

    | _, Value.Closure _ -> None

    | _, Value.Comodel _ -> None

    | Syntax.PattNumeral _, (Value.Boolean _ | Value.Constructor _ | Value.Tuple _)
      | Syntax.PattBoolean _, (Value.Numeral _ | Value.Constructor _ | Value.Tuple _)
      | Syntax.PattConstructor _, (Value.Boolean _ | Value.Numeral _ | Value.Tuple _)
      | Syntax.PattTuple _, (Value.Numeral _ | Value.Boolean _ | Value.Constructor _) ->
       None

  and fold_tuple us ps vs =
    match ps, vs with
    | [], [] -> Some us
    | p :: ps, v :: vs ->
       begin
         match fold us p v with
         | None -> None
         | Some us -> fold_tuple us ps vs
       end
    | [], _::_ | _::_, [] -> None
  in

  match fold [] p v with
  | None -> None
  | Some us -> Some (List.rev us)


let extend_pattern ~loc p v env =
  match match_pattern p v with
  | None -> error ~loc PatternMismatch
  | Some us -> extends us env

let top_extend_pattern ~loc p v env =
  match match_pattern p v with
  | None -> error ~loc PatternMismatch
  | Some us -> extends us env, us

let match_clauses ~loc env ps v =
  let rec fold = function
    | [] -> error ~loc PatternMismatch
    | (p, c) :: lst ->
       begin
         match match_pattern p v with
         | None -> fold lst
         | Some us -> (extends us env, c)
       end
  in
  fold ps

let as_pair ~loc = function
  | Value.Tuple [v1; v2] -> (v1, v2)
  | Value.Closure _ | Value.Numeral _ | Value.Boolean _ | Value.Constructor _
    | Value.Tuple ([] | [_] | _::_::_::_) | Value.Comodel _ ->
     error ~loc PairExpected

let as_closure ~loc = function
  | Value.Closure f -> f
  | Value.Numeral _ | Value.Boolean _ | Value.Constructor _ | Value.Tuple _
    | Value.Comodel _ -> error ~loc FunctionExpected

let as_value ~loc = function
  | Value.Val v -> v
  | Value.Operation (op, _, _) -> error ~loc (UnhandledOperation op)
  | Value.Signal (sgl, _) -> error ~loc (UnhandledSignal sgl)

let as_comodel ~loc = function
  | Value.Comodel (w, cmdl) -> (w, cmdl)
  | Value.Numeral _ | Value.Boolean _ | Value.Tuple _ | Value.Constructor _
    | Value.Closure _ -> error ~loc ComodelExpected

(** The result monad *)

let rec bind r k =
  match r with
  | Value.Val v -> k v
  | Value.Operation (op, u, l) -> Value.Operation (op, u, fun x -> let r = l x in bind r k)
  | Value.Signal _ as v -> v

let ( >>= ) = bind

(*** Evaluation ***)

let rec eval_expr env {Location.it=e'; loc} =
  match e' with

  | Syntax.Numeral k -> Value.Numeral k

  | Syntax.Boolean b -> Value.Boolean b

  | Syntax.Var i -> lookup ~loc i env

  | Syntax.Constructor (cnstr, None) ->
     Value.Constructor (cnstr, None)

  | Syntax.Constructor (cnstr, Some e) ->
     let v = eval_expr env e in
     Value.Constructor (cnstr, Some v)

  | Syntax.Tuple lst ->
     let lst = List.map (eval_expr env) lst in
     Value.Tuple lst

  | Syntax.Lambda (p, c) ->
     let f v =
       let env = extend_pattern ~loc p v env in
       eval_comp env c
     in
     Value.Closure f

  | Syntax.Comodel (e, lst) ->
     let w = eval_expr env e in
     let coop px pw c =
       let loc = c.Location.loc in
       fun (u, w) ->
       let env = extend_pattern ~loc px u env in
       let env = extend_pattern ~loc pw w env in
       eval_comp env c >>= fun u ->
       let (v, w) = as_pair ~loc u in
       Value.Val (v, w)
     in
     let cmdl =
       List.fold_left
         (fun cmdl (op, px, pw, c) -> Name.Map.add op (coop px pw c) cmdl)
         Name.Map.empty
         lst
     in
     Value.Comodel (w, cmdl)

  | Syntax.ComodelTimes (e1, e2) ->
     let wrap_fst f (v, w) =
         let (w1, w2) = as_pair ~loc w in
         f (v, w1) >>= fun (v', w1') ->
         let w' = Value.Tuple [w1'; w2] in
         Value.Val (v', w')
     in
     let wrap_snd f (v, w) =
         let (w1, w2) = as_pair ~loc w in
         f (v, w2) >>= fun (v', w2') ->
         let w' = Value.Tuple [w1; w2'] in
         Value.Val (v', w')
     in
     let (w1, cmdl1) = as_comodel ~loc (eval_expr env e1)
     and (w2, cmdl2) = as_comodel ~loc (eval_expr env e2) in
     let w = Value.Tuple [w1; w2] in
     let cmdl =
       Name.Map.merge
         (fun op f1 f2 ->
           match f1, f2 with
           | None, None -> None
           | Some f1, None -> Some (wrap_fst f1)
           | None, Some f2 -> Some (wrap_snd f2)
           | Some _, Some _ -> error ~loc (ComodelDoubleOperation op))
         cmdl1 cmdl2
     in
     Value.Comodel (w, cmdl)

  | Syntax.ComodelRename (e, rnm) ->
     let (w, cmdl) = as_comodel ~loc (eval_expr env e) in
     let cmdl =
       Name.Map.fold
         (fun op f cmdl ->
           match Name.Map.find op rnm with
           | None -> error ~loc (IllegalRenaming op)
           | Some op' -> Name.Map.add op' f cmdl)
         cmdl Name.Map.empty
     in
     Value.Comodel (w, cmdl)

and eval_comp env {Location.it=c'; loc} =
  match c' with

  | Syntax.Val e ->
     let v = eval_expr env e in
     Value.Val v

  | Syntax.Match (e, lst) ->
     let v = eval_expr env e in
     let env, c = match_clauses ~loc env lst v in
     eval_comp env c

  | Syntax.Apply (e1, e2) ->
     let v1 = eval_expr env e1 in
     let f = as_closure ~loc v1 in
     let v2 = eval_expr env e2 in
     f v2

  | Syntax.Let (p, c1, c2) ->
     eval_comp env c1 >>= fun v ->
     eval_comp (extend_pattern ~loc p v env) c2

  | Syntax.LetRec (fs, c) ->
     let env = extend_rec ~loc fs env in
     eval_comp env c

  | Syntax.Operation (op, u) ->
     let u = eval_expr env u in
     Value.Operation (op, u, (fun v -> Value.Val v))

  | Syntax.Signal (sgl, e) ->
     let v = eval_expr env e in
     Value.Signal (sgl, v)

  | Syntax.Using (e, c, fin) ->
     let (w, cmdl) = as_comodel ~loc (eval_expr env e)
     and fin = eval_finally ~loc env fin
     and r = eval_comp env c in
     using ~loc env cmdl w r fin

and eval_finally ~loc env {Syntax.fin_val=(px, pw, c); Syntax.fin_signals=fin_signals} =
  let fin_val (v, w) =
    let env = extend_pattern ~loc px v env in
    let env = extend_pattern ~loc pw w env in
    eval_comp env c
  and fin_signals =
    List.fold_left
      (fun fin_signals (sgl, px, pw, c) ->
        let f (v, w) =
          let env = extend_pattern ~loc px v env in
          let env = extend_pattern ~loc pw w env in
          eval_comp env c
        in
        Name.Map.add sgl f fin_signals)
      Name.Map.empty
      fin_signals
  in
  (fin_val, fin_signals)

and extend_rec ~loc fs env =
  let env' = ref env in
  let mk_closure (p, c) =
    Value.Closure (fun v -> eval_comp (extend_pattern ~loc p v !env') c)
  in
  let fs = List.map mk_closure fs in
  let env = extends fs env in
  env' := env ;
  env

and using ~loc env cmdl w r (fin_val, fin_signals) =
  let rec tensor w r =
    match r with

    | Value.Val v -> fin_val (v, w)

    | Value.Operation (op, u, k) ->
       begin
         match Name.Map.find op cmdl with
         | None -> error ~loc (UnhandledOperation op)
         | Some coop ->
            let rec let_unless = function
              | Value.Val _ as v -> v

              | Value.Operation (op, u, k) ->
                 Value.Operation (op, u, (fun v -> let_unless (k v)))

              | Value.Signal (sgl, v) ->
                 begin
                   match Name.Map.find sgl fin_signals with
                   | None -> error ~loc (UnhandledSignal sgl)
                   | Some f -> f (v, w)
                 end
            in
            let r = coop (u, w) >>= fun (v, w') -> tensor w' (k v) in
            let_unless r
       end

    | Value.Signal (sgl, v) as r ->
       begin
         match Name.Map.find sgl fin_signals with
         | None -> r
         | Some f -> f (v, w)
       end
  in
  tensor w r

let rec eval_toplevel ~quiet env {Location.it=d'; loc} =
  match d' with

  | Syntax.TopLoad cs ->
     eval_topfile ~quiet env cs

  | Syntax.TopLet (p, xts, c) ->
     let r = eval_comp env c in
     let v = as_value ~loc r in
     let env, vs = top_extend_pattern ~loc p v env in
     if not quiet then
       List.iter2
         (fun (x, ty) v ->
           Format.printf "@[<hov>val %t@ :@ %t@ =@ %t@]@."
                         (Name.print x)
                         (Syntax.print_expr_ty ty)
                         (Value.print v))
         xts vs ;
     env

  | Syntax.TopLetRec (pcs, fts) ->
     let env = extend_rec ~loc pcs env in
     if not quiet then
       List.iter
         (fun (f, t) ->
           Format.printf "@[<hov>val %t@ :@ %t@ =@ <fun>@]@."
                         (Name.print f)
                         (Syntax.print_expr_ty t))
         fts ;
     env

  | Syntax.TopComp (c, ty) ->
     let r = eval_comp env c in
     let v = as_value ~loc r in
     if not quiet then
       Format.printf "@[<hov>- :@ %t@ =@ %t@]@."
                     (Syntax.print_expr_ty ty)
                     (Value.print v) ;
     env

  | Syntax.DefineAbstract t ->
     if not quiet then
       Format.printf "@[<hov>type %t@]@." (Name.print t) ;
     env

  | Syntax.DefineAlias (t, abbrev) ->
     if not quiet then
       Format.printf "@[<hov>type %t@ =@ %t@]@."
                     (Name.print t)
                     (Syntax.print_expr_ty abbrev) ;
     env

  | Syntax.DefineDatatype lst ->
     if not quiet then
       Format.printf "@[<v 5>type %t@]@."
                     (Print.sequence (Syntax.print_datatype) " and" lst) ;
     env

  | Syntax.DeclareOperation (op, ty1, ty2) ->
     if not quiet then
       Format.printf "@[<hov>operation %t@ :@ %t@ %s@ %t@]@."
                     (Name.print op)
                     (Syntax.print_expr_ty ty1)
                     (Print.char_arrow ())
                     (Syntax.print_expr_ty ty2) ;
     env

  | Syntax.DeclareSignal (sgl, t) ->
     if not quiet then
       Format.printf "@[<hov>signal %t@ of@ %t@]@."
                     (Name.print sgl)
                     (Syntax.print_expr_ty t) ;
     env


  | Syntax.External (x, t, s) ->
     begin
       match External.lookup s with
       | None -> error ~loc (UnknownExternal s)
       | Some v ->
          let env = extend v env in
          if not quiet then
            Format.printf "@[<hov>external %t@ :@ %t = \"%s\"@]@."
                          (Name.print x)
                          (Syntax.print_expr_ty t)
                          s ;
          env
     end

and eval_topfile ~quiet env lst =
  let rec fold env = function
    | [] -> env
    | top_cmd :: lst ->
       let env = eval_toplevel ~quiet env top_cmd in
       fold env lst
  in
  fold env lst
