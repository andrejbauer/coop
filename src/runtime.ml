type environment = {
    env_vars : Value.t list ;
    env_shell : Value.shell
  }

type error =
  | InvalidDeBruijn of int
  | UnhandledOperation of Name.t * Value.t
  | UnhandledSignal of Name.t * Value.t
  | UnknownExternal of string
  | IllegalRenaming of Name.t
  | IllegalComparison of Value.t
  | FunctionExpected
  | RunnerExpected
  | RunnerDoubleOperation of Name.t
  | PairExpected
  | ShellExpected
  | PatternMismatch

exception Error of error Location.located

let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

let print_error err ppf =
  match err with

  | InvalidDeBruijn i ->
     Format.fprintf ppf "invalid de Bruijn index %d, please report" i

  | UnhandledOperation (op, v) ->
     Format.fprintf ppf "unhandled operation %t@ %t"
       (Name.print op)
       (Value.print ~max_level:Level.constr_arg v)

  | UnhandledSignal (sgl, v) ->
     Format.fprintf ppf "terminated by signal %t@ %t"
       (Name.print sgl)
       (Value.print ~max_level:Level.constr_arg v)

  | UnknownExternal s ->
     Format.fprintf ppf "unknown external %s" s

  | IllegalRenaming op ->
     Format.fprintf ppf "illegal runner renaming %t, please report" (Name.print op)

  | IllegalComparison v ->
     Format.fprintf ppf "cannot compare %s" (Value.names v)

  | FunctionExpected ->
     Format.fprintf ppf "function expected, please report"

  | RunnerExpected ->
     Format.fprintf ppf "runner expected, please report"

  | RunnerDoubleOperation op ->
     Format.fprintf ppf "cannot combine models that both contain the coperation %t" (Name.print op)

  | PairExpected ->
     Format.fprintf ppf "pair expected, please report"

  | ShellExpected ->
     Format.fprintf ppf "shell expected, please report"

  | PatternMismatch ->
     Format.fprintf ppf "pattern mismatch"

let initial = {
    env_vars = [] ;
    env_shell = Value.pure_shell
}

let set_shell env_shell env =
  { env with env_shell }

(** Extend the variables with a variable *)
let extend_var v vars = v :: vars

let rec extend_vars vs vars =
  match vs with
  | [] -> vars
  | v :: vs ->
     let vars = extend_var v vars in
     extend_vars vs vars

let lookup_var ~loc i vars =
  try
    List.nth vars i
  with
  | Failure _ -> error ~loc (InvalidDeBruijn i)

let match_pattern p v =
  let rec fold us p v =
    match p with

    | Syntax.PattAnonymous -> Some us

    | Syntax.PattVar ->
       Some (v :: us)

    | Syntax.PattNumeral m ->
        begin match v with
        | Value.Numeral n when m = n -> Some us
        | _ -> None
        end

    | Syntax.PattBoolean b ->
       begin match v with
       | Value.Boolean b' when b = b' -> Some us
       | _ -> None
       end

    | Syntax.PattQuoted s ->
       begin match v with
       | Value.Quoted s' -> if String.equal s s' then Some us else None
       | _ -> None
       end

    | Syntax.PattConstructor (cnstr, popt) ->
       begin match v with
       | Value.Constructor (cnstr', vopt) when Name.equal cnstr cnstr' ->
          begin
           match popt, vopt with
           | None, None -> Some us
           | Some p, Some v -> fold us p v
           | Some _, None | None, Some _ -> None
          end
       | _ -> None
       end

    | Syntax.PattTuple ps ->
       begin match v with
       | Value.Tuple vs -> fold_tuple us ps vs
       | _ -> None
       end

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
  | Some us -> extend_vars us env

let top_extend_pattern ~loc p v env =
  match match_pattern p v with
  | None -> error ~loc PatternMismatch
  | Some us -> extend_vars us env, us

let match_clauses ~loc env ps v =
  let rec fold = function
    | [] -> error ~loc PatternMismatch
    | (p, c) :: lst ->
       begin
         match match_pattern p v with
         | None -> fold lst
         | Some us -> (extend_vars us env, c)
       end
  in
  fold ps

let as_pair ~loc = function
  | Value.Tuple [v1; v2] -> (v1, v2)
  | Value.(Closure _ | Numeral _ | Boolean _ | Quoted _ | Constructor _
    | Tuple ([] | [_] | _::_::_::_) | Runner _ | Abstract | Shell _) ->
     error ~loc PairExpected

let as_closure ~loc = function
  | Value.Closure f -> f
  | Value.(Numeral _ | Boolean _ | Quoted _ | Constructor _ | Tuple _ |
    Runner _  | Abstract | Shell _) ->
     error ~loc FunctionExpected

let as_runner ~loc = function
  | Value.Runner cmdl -> cmdl
  | Value.(Numeral _ | Boolean _ | Quoted _ | Tuple _ | Constructor _ |
           Closure _  | Abstract | Shell _) ->
     error ~loc RunnerExpected

let as_shell ~loc = function
  | Value.Shell ops -> ops
  | Value.(Numeral _ | Boolean _ | Quoted _ | Tuple _ | Constructor _ |
           Closure _  | Abstract | Runner _) ->
     error ~loc ShellExpected


(** Comparison of values *)
let rec equal_value ~loc (v1 : Value.t) (v2 : Value.t) =
  match v1, v2 with

  | Value.(Abstract | Closure _ | Runner _ | Shell _), _ ->
     error ~loc (IllegalComparison v1)

  | _, Value.(Abstract | Closure _ | Runner _) ->
     error ~loc (IllegalComparison v1)

  | Value.(Numeral k1, Numeral k2) ->
     k1 = k2

  | Value.(Boolean b1, Boolean b2) ->
     b1 = b2

  | Value.(Quoted s1, Quoted s2) ->
     String.equal s1 s2

  | Value.(Constructor (cnstr1, v1opt), Constructor (cnstr2, v2opt)) ->
     Name.equal cnstr1 cnstr2 && equal_value_opt ~loc v1opt v2opt

 | (Value.Tuple lst1, Value.Tuple lst2) ->
    equal_values ~loc lst1 lst2

 | Value.(Numeral _ | Boolean _ | Quoted _ | Constructor _ | Tuple _), _ ->
    false

and equal_value_opt ~loc v1opt v2opt =
  match v1opt, v2opt with
  | None, None -> true
  | Some v1, Some v2 -> equal_value ~loc v1 v2
  | None, Some _ | Some _, None -> false

and equal_values ~loc vs1 vs2 =
  match vs1, vs2 with
  | [], [] -> true
  | v1 :: vs1, v2 :: vs2 -> equal_value ~loc v1 v2 && equal_values ~loc vs1 vs2
  | [], _::_ | _::_, [] -> false

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

  | Syntax.Quoted s -> Value.Quoted s

  | Syntax.Var i -> lookup_var ~loc i env

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

  | Syntax.Runner lst ->
     let coop px pw c =
       let loc = c.Location.loc in
       fun (u, Value.World w) ->
       let env = extend_pattern ~loc px u env in
       let env = extend_pattern ~loc pw w env in
       eval_comp env c >>= fun u ->
       let (v, w) = as_pair ~loc u in
       Value.(Val (v, World w))
     in
     let cmdl =
       List.fold_left
         (fun cmdl (op, px, pw, c) -> Name.Map.add op (coop px pw c) cmdl)
         Name.Map.empty
         lst
     in
     Value.(Runner cmdl)

  | Syntax.RunnerTimes (e1, e2) ->
     let wrap_fst f (v, Value.World w) =
         let (w1, w2) = as_pair ~loc w in
         f (v, Value.World w1) >>= fun (v', Value.World w1') ->
         let w' = Value.Tuple [w1'; w2] in
         Value.Val (v', Value.World w')
     in
     let wrap_snd f (v, Value.World w) =
         let (w1, w2) = as_pair ~loc w in
         f (v, Value.World w2) >>= fun (v', Value.World w2') ->
         let w' = Value.Tuple [w1; w2'] in
         Value.Val (v', Value.World w')
     in
     let cmdl1 = as_runner ~loc (eval_expr env e1)
     and cmdl2 = as_runner ~loc (eval_expr env e2) in
     let cmdl =
       Name.Map.merge
         (fun op f1 f2 ->
           match f1, f2 with
           | None, None -> None
           | Some f1, None -> Some (wrap_fst f1)
           | None, Some f2 -> Some (wrap_snd f2)
           | Some _, Some _ -> error ~loc (RunnerDoubleOperation op))
         cmdl1 cmdl2
     in
     Value.Runner cmdl

  | Syntax.RunnerRename (e, rnm) ->
     let cmdl = as_runner ~loc (eval_expr env e) in
     let cmdl =
       Name.Map.fold
         (fun op f cmdl ->
           match Name.Map.find op rnm with
           | None -> error ~loc (IllegalRenaming op)
           | Some op' -> Name.Map.add op' f cmdl)
         cmdl Name.Map.empty
     in
     Value.Runner cmdl

and eval_comp env {Location.it=c'; loc} =
  match c' with

  | Syntax.Val e ->
     let v = eval_expr env e in
     Value.Val v

  | Syntax.Match (e, lst) ->
     let v = eval_expr env e in
     let env, c = match_clauses ~loc env lst v in
     eval_comp env c

  | Syntax.Equal (e1, e2) ->
     let v1 = eval_expr env e1
     and v2 = eval_expr env e2 in
     let b = equal_value ~loc v1 v2 in
     Value.Val (Value.Boolean b)

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

  | Syntax.Run (e1, e2, c, fin) ->
     let cmdl = as_runner ~loc (eval_expr env e1)
     and w = eval_expr env e2
     and fin = eval_finally ~loc env fin
     and r = eval_comp env c in
     use ~loc cmdl (Value.World w) r fin

  | Syntax.Try _ ->
     failwith "evaluation of try is not implemented"

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
  let env = extend_vars fs env in
  env' := env ;
  env

and use ~loc cmdl w r (fin_val, fin_signals) =
  let rec tensor (Value.World w' as w) r =
    match r with

    | Value.Val v -> fin_val (v, w')

    | Value.Operation (op, u, k) ->
       begin
         match Name.Map.find op cmdl with
         | None -> error ~loc (UnhandledOperation (op, u))
         | Some coop ->
            let rec let_unless = function
              | Value.Val _ as v -> v

              | Value.Operation (op, u, k) ->
                 Value.Operation (op, u, (fun v -> let_unless (k v)))

              | Value.Signal (sgl, v) ->
                 begin
                   match Name.Map.find sgl fin_signals with
                   | None -> error ~loc (UnhandledSignal (sgl, v))
                   | Some f -> f (v, w')
                 end
            in
            let r = coop (u, w) >>= fun (v, w') -> tensor w' (k v) in
            let_unless r
       end

    | Value.Signal (sgl, v) as r ->
       begin
         match Name.Map.find sgl fin_signals with
         | None -> r
         | Some f -> f (v, w')
       end
  in
  tensor w r

let top_eval_comp {env_vars; env_shell=(coops, w)} ({Location.loc; _} as c) =
  let rec tensor w r =
    match r with
    | Value.Val v -> v

    | Value.Operation (op, u, k) ->
       begin
         match Name.Map.find op coops with
         | None -> error ~loc (UnhandledOperation (op, u))
         | Some coop ->
            let (v, w) = coop (u, w) in
            let r = k v in
            tensor w r
       end

    | Value.Signal (sgl, v) -> error ~loc (UnhandledSignal (sgl, v))
  in
  tensor w (eval_comp env_vars c)


let rec eval_toplevel ~quiet ({env_vars; env_shell} as env) {Location.it=d'; loc} =
  match d' with

  | Syntax.TopLoad cs ->
     eval_topfile ~quiet {env_vars; env_shell} cs

  | Syntax.TopLet (p, xts, c) ->
     let v = top_eval_comp env c in
     let env_vars, vs = top_extend_pattern ~loc p v env_vars in
     if not quiet then
       List.iter2
         (fun (x, ty) v ->
           Format.printf "@[<hv 2>val %t :@ @[<hov>%t@]@ @[<hov 2>= %t@]@]@."
                         (Name.print x)
                         (Syntax.print_expr_ty ty)
                         (Value.print v))
         xts vs ;
     { env with env_vars }

  | Syntax.TopLetRec (pcs, fts) ->
     let env_vars = extend_rec ~loc pcs env_vars in
     if not quiet then
       List.iter
         (fun (f, t) ->
           Format.printf "@[<hov>val %t@ :@ %t@ =@ <fun>@]@."
                         (Name.print f)
                         (Syntax.print_expr_ty t))
         fts ;
     { env with env_vars }

  | Syntax.TopComp (c, ty) ->
     let v = top_eval_comp env c in
     if not quiet then
       Format.printf "@[<hov>- :@ %t@ =@ %t@]@."
                     (Syntax.print_expr_ty ty)
                     (Value.print v) ;
     env

  | Syntax.TopShell (c, ops) ->
     let v = top_eval_comp env c in
     let shl = as_shell ~loc v in
     let env = set_shell shl env in
     if not quiet then
       Format.printf "@[<hov>shell@ {%t}@]@." (Syntax.print_shell_ty ops) ;
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
       Format.printf "@[<v>type %t@]@." (Syntax.print_datatypes lst) ;
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
          let env_vars = extend_var v env_vars in
          if not quiet then
            Format.printf "@[<hov>external %t@ :@ %t = \"%s\"@]@."
                          (Name.print x)
                          (Syntax.print_expr_ty t)
                          s ;
          { env with env_vars }
     end

and eval_topfile ~quiet env lst =
  let rec fold env = function
    | [] -> env
    | top_cmd :: lst ->
       let env = eval_toplevel ~quiet env top_cmd in
       fold env lst
  in
  fold env lst
