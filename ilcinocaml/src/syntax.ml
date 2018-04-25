(* -------------------------------------------------------------------------- *)
(* Abstract syntax tree *)

open Format
   
type name = string

(* Types *)
type ty = 
  | TyInt
  | TyBool
  | TyString
  | TyArrow of ty * ty

type pattern =
  | PatName of name
  | PatImpName of name
  | PatDeref of name
  | PatTag of string
  | PatInt of int
  | PatBool of bool
  | PatString of string
  | PatList of pattern list
  | PatUnit
  | PatWildcard
  | PatTuple of pattern list
  | PatCons of pattern * pattern
    
type expr =
  | Name of name
  | ImpName of name
  | Tag of string
  | Int of int
  | Bool of bool
  | String of string
  | List of expr list
  | Set of expr list
  | Tuple of expr list
  | Wildcard
  | Unit
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Divide of expr * expr
  | Mod of expr * expr
  | Or of expr * expr
  | And of expr * expr
  | Not of expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Leq of expr * expr
  | Geq of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Let of pattern * expr * expr
  | LetRec of name * expr * expr
  | Assign of pattern * expr
  | Ref of expr
  | Deref of expr
  | Match of expr * (pattern * expr) list
  | IfTE of expr * expr * expr
  | IfT of expr * expr
  | Req of expr * expr
  | Lam of expr * expr
  | App of expr * expr
  | Wr of expr * expr
  | Rd of expr
  | Nu of name * expr
  | Repl of expr
  | ParComp of expr * expr
  | ParLeft of expr * expr
  | Fork of expr
  | Choice of expr * expr
  | Seq of expr * expr
  | Thunk of expr
  | Force of expr
  | Fst of expr
  | Snd of expr
  | Rand
  | GetBit
  | Show of expr
  | Cons of expr * expr
  | Concat of expr * expr
  | Lookup of expr * expr
  | Length of expr
  | Mem of expr * expr
  | Union of expr * expr
  | Print of expr
  | Rev of expr
  
type process =
  | Process of expr

(* -------------------------------------------------------------------------- *)
(* Printing *)

let rec pr_expr ppf = function
  | Name s -> fprintf ppf "%s(%s)" "Name" s
  | ImpName s -> fprintf ppf "%s(%s)" "ImpName" s
  | Tag s -> fprintf ppf "%s(%s)" "Tag" s
  | Int n -> fprintf ppf "%s(%d)" "Int" n
  | Bool b -> fprintf ppf "%s(%b)" "Bool" b
  | String s -> fprintf ppf "%s(\"%s\")" "String" s
  | List es -> fprintf ppf "@[<2>%s(%a)@]" "List" pr_list es
  | Set es -> fprintf ppf "@[<2>%s(%a)@]" "Set" pr_list es
  | Tuple es -> fprintf ppf "@[<2>%s(%a)@]" "Tuple" pr_list es
  | Wildcard -> fprintf ppf "Wildcard"
  | Unit -> fprintf ppf "Unit"
  | Plus (e1, e2) -> pr_binop ppf "Plus" e1 e2
  | Minus (e1, e2) -> pr_binop ppf "Minus" e1 e2
  | Times (e1, e2) -> pr_binop ppf "Times" e1 e2
  | Divide (e1, e2) -> pr_binop ppf "Divide" e1 e2
  | Mod (e1, e2) -> pr_binop ppf "Mod" e1 e2
  | Or (e1, e2) -> pr_binop ppf "Or" e1 e2
  | And (e1, e2) -> pr_binop ppf "And" e1 e2
  | Not e -> fprintf ppf "@[<2>%s(%a)@]" "Not" pr_expr e
  | Lt (e1, e2) -> pr_binop ppf "Lt" e1 e2
  | Gt (e1, e2) -> pr_binop ppf "Gt" e1 e2
  | Leq (e1, e2) -> pr_binop ppf "Leq" e1 e2
  | Geq (e1, e2) -> pr_binop ppf "Geq" e1 e2
  | Eq (e1, e2) -> pr_binop ppf "Eq" e1 e2
  | Neq (e1, e2) -> pr_binop ppf "Neq" e1 e2
  | Let (p, e1, e2) ->
     fprintf ppf "@[<v>@[<v 2>%s(@,%a,@,%a,@,%a@]@,)@]" "Let"
       pr_pat p pr_expr e1 pr_expr e2
  | LetRec (s, e1, e2) ->
     fprintf ppf "@[<v>@[<v 2>%s(@,%s,@,%a,@,%a@]@,)@]" "LetRec"
       s pr_expr e1 pr_expr e2
  | Assign (p, e1) ->
     fprintf ppf "@[<v>@[<v 2>%s(@,%a,@,%a@]@,)@]" "Assign"
       pr_pat p pr_expr e1
  | Ref e -> fprintf ppf "%s(%a)" "Ref" pr_expr e
  | Deref e -> fprintf ppf "%s(%a)" "Deref" pr_expr e
  | Match (e, bs) ->
     fprintf ppf "@[<v>@[<v 2>%s(@,%a@,%a@]@,)@]" "Match"
       pr_expr e pr_list_bs bs
  | IfTE (e1, e2, e3) ->
     fprintf ppf "@[<v>@[<v 2>%s(@,%a,@,%a,@,%a@]@,)@]" "IfTE"
       pr_expr e1 pr_expr e2 pr_expr e3
  | IfT (e1, e2) ->
     fprintf ppf "@[<v>@[<v 2>%s(@,%a,@,%a@]@,)@]" "IfT"
       pr_expr e1 pr_expr e2 
  | Req (e1, e2) -> pr_binop ppf "Req" e1 e2
  | Lam (e1, e2) -> pr_binop ppf "Lam" e1 e2
  | App (e1, e2) -> pr_binop ppf "App" e1 e2
  | Wr (e1, e2) -> pr_binop ppf "Wr" e1 e2
  | Rd e -> fprintf ppf "@[<2>%s(%a)@]" "Rd" pr_expr e
  | Nu (s, e) -> fprintf ppf "@[<v>@[<v 2>%s(@,%s,@,%a@]@,)@]" "Nu"
                   s pr_expr e
  | Repl e -> fprintf ppf "%s(%a)" "Repl" pr_expr e
  | ParComp (e1, e2) -> pr_binop ppf "ParComp" e1 e2
  | ParLeft (e1, e2) -> pr_binop ppf "ParLeft" e1 e2
  | Fork e -> fprintf ppf "@[<2>%s(%a)@]" "Fork" pr_expr e
  | Choice (e1, e2) -> pr_binop ppf "Choice" e1 e2
  | Seq (e1, e2) -> pr_binop ppf "Seq" e1 e2
  | Thunk e -> fprintf ppf "@[<2>%s(%a)@]" "Thunk" pr_expr e
  | Force e -> fprintf ppf "@[<2>%s(%a)@]" "Force" pr_expr e
  | Fst e -> fprintf ppf "@[<2>%s(%a)@]" "Fst" pr_expr e
  | Snd e -> fprintf ppf "@[<2>%s(%a)@]" "Snd" pr_expr e
  | Rand -> fprintf ppf "Rand()"
  | GetBit -> fprintf ppf "GetBit()"
  | Show e -> fprintf ppf "@[<2>%s(%a)@]" "Show" pr_expr e
  | Cons (e1, e2) -> pr_binop ppf "Cons" e1 e2
  | Concat (e1, e2) -> pr_binop ppf "Concat" e1 e2
  | Lookup (e1, e2) -> pr_binop ppf "Lookup" e1 e2
  | Length e -> fprintf ppf "@[<2>%s(%a)@]" "Length" pr_expr e
  | Mem (e1, e2) -> pr_binop ppf "Mem" e1 e2
  | Union (e1, e2) -> pr_binop ppf "Union" e1 e2
  | Print e -> fprintf ppf "@[<2>%s(%a)@]" "Print" pr_expr e
  | Rev e -> fprintf ppf "@[<2>%s(%a)@]" "Rev" pr_expr e
and pr_list ppf = function
  | [x] -> fprintf ppf "%a" pr_expr x
  | x::xs -> fprintf ppf "%a,@ %a" pr_expr x pr_list xs
  | _ -> fprintf ppf ""
and pr_listp ppf = function
  | [x] -> fprintf ppf "%a" pr_pat x
  | x::xs -> fprintf ppf "%a,@ %a" pr_pat x pr_listp xs
  | _ -> fprintf ppf ""
and pr_list_bs ppf = function
  | [x] -> fprintf ppf "%a" pr_b x
  | x::xs -> fprintf ppf "%a,@ %a" pr_b x pr_list_bs xs
  | _ -> fprintf ppf ""
and pr_b ppf = function
  | (p, e) ->
     fprintf ppf "@[<v>@[<v 2>%s(@,%a,@,%a@]@,)@]" "Branch"
       pr_pat p pr_expr e
and pr_binop ppf op lhs rhs =
  fprintf ppf "@[<v>@[<v 2>%s(@,%a,@,%a@]@,)@]" op
    pr_expr lhs pr_expr rhs
and pr_pat ppf = function
  | PatName s -> fprintf ppf "%s(%s)" "PName" s
  | PatDeref s -> fprintf ppf "%s(%s)" "PDeref" s
  | PatImpName s -> fprintf ppf "%s(%s)" "PImpName" s
  | PatTag s -> fprintf ppf "%s(%s)" "PTag" s
  | PatInt s -> fprintf ppf "%s(%d)" "PInt" s
  | PatBool b -> fprintf ppf "%s(%b)" "PBool" b
  | PatString s -> fprintf ppf "%s(\"%s\")" "PImpName" s
  | PatList ps -> fprintf ppf "@[<2>%s(%a)@]" "PList" pr_listp ps
  | PatTuple ps -> fprintf ppf "@[<2>%s(%a)@]" "PTuple" pr_listp ps
  | PatWildcard -> fprintf ppf "PWildcard"
  | PatUnit -> fprintf ppf "PUnit"
  | PatCons (hd, tl) -> 
     fprintf ppf "@[<v>@[<v 2>%s(@,%a,@,%a@]@,)@]" "PCons"
       pr_pat hd pr_pat tl

let print_ast = pr_expr std_formatter           
