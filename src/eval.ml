let rec expr env {Location.data=e'; loc} =
  match e' with

  | Rsyntax.Numeral k -> Runtime.Numeral k

  | Rsyntax.Var i -> Runtime.lookup ~loc i env

  | Rsyntax.Tuple lst ->
     let lst = List.map (expr env) lst in
     Runtime.Tuple lst

  | Rsyntax.Lambda c ->
     let f v =
       let env = Runtime.extend v env in
       comp env c
     in
     Runtime.Closure f


and comp env {Location.data=c'; loc} =
  match c' with

  | Rsyntax.Return e ->
     let v = expr env e in
     Runtime.Return v

  | Rsyntax.Match (e, lst) ->
     let v = expr env e in
     let env, c = Runtime.match_clauses ~loc env lst v in
     comp env c

  | Rsyntax.Apply (e1, e2) ->
     let v1 = expr env e1 in
     let f = Runtime.as_closure ~loc v1 in
     let v2 = expr env e2 in
     f v2

  | Rsyntax.Sequence (c1, c2) ->
     begin
       match comp env c1 with

       | Runtime.Return v ->
          comp (Runtime.extend v env) c2

       | Runtime.Operation (op, u, k) ->
          Runtime.Operation (op, u, (fun v -> comp (Runtime.extend v env) c2))
     end

let rec toplevel ~quiet env {Location.data=d'; loc} =
  match d' with

  | Rsyntax.TopLoad cs ->
     topfile ~quiet env cs

  | Rsyntax.TopLet (x, ty, c) ->
     let r = comp env c in
     let v = Runtime.as_value ~loc r in
     if not quiet then
       Format.printf "@[<hov>val %t@ :@ %t@ =@ %t@]@."
         (Name.print_ident x)
         (Rsyntax.print_expr_ty ty)
         (Runtime.print_value v) ;
     Runtime.extend v env

  | Rsyntax.TopComp (c, ty) ->
     let r = comp env c in
     let v = Runtime.as_value ~loc r in
     if not quiet then
       Format.printf "@[<hov>- :@ %t@ =@ %t@]@."
         (Rsyntax.print_comp_ty ty)
         (Runtime.print_value v) ;
     env

  | Rsyntax.DeclOperation (op, ty1, ty2) ->
     if not quiet then
       Format.printf "@[<hov>operation@ %t@ :@ %t@ %s@ %t@]@."
         (Name.print_ident op)
         (Rsyntax.print_expr_ty ty1)
         (Print.char_arrow ())
         (Rsyntax.print_comp_ty ty2) ;
     Runtime.extend (Runtime.generic op) env


and topfile ~quiet env lst =
  let rec fold env = function
    | [] -> env
    | top_cmd :: lst ->
       let env = toplevel ~quiet env top_cmd in
       fold env lst
  in
  fold env lst
