let rec expr env {Location.data=e'; loc} =
  match e' with

  | Rsyntax.Numeral k -> Runtime.Numeral k

  | Rsyntax.Var i -> Runtime.lookup ~loc i env

  | Rsyntax.Lambda c ->
     let f v =
       let env = Runtime.extend v env in
       comp env c
     in
     Runtime.Closure f


and comp env {Location.data=c'; _} =
  match c' with
  | Rsyntax.Return e ->
     let v = expr env e in
     Runtime.Return v

  | Rsyntax.Apply (_, _) -> failwith "evaluation does not work yet"

  | Rsyntax.Sequence (_, _) -> failwith "evaluation does not work yet"


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

and topfile ~quiet env lst =
  let rec fold env = function
    | [] -> env
    | top_cmd :: lst ->
       let env = toplevel ~quiet env top_cmd in
       fold env lst
  in
  fold env lst
