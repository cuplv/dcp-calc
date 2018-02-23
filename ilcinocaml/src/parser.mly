%{
  open Syntax
  
  exception Parsing_error
  
  let get_names = function 
    | Name x -> x
    | _ -> raise Parsing_error

  let curry_lambdas x acc =
    match x with
    | x -> Lam (x, acc)

  let curry acc e = App(acc, e)

  let desugar_let acc map =
    match map with
    | (pattern, expr) -> Let(pattern, expr, acc)
%}

/* Identifier and constants */
%token <Syntax.name> NAME
%token <Syntax.name> IMPNAME
%token <string> TAG
%token <int> INT
%token <string> STRING 

/* Reserved words */
%token LET
%token LETREC
%token IN
%token MATCH
%token WITH
%token LAM
%token NU
%token WR
%token RD
%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token THUNK
%token FORCE
%token REQ
%token END
%token UNIT

/* Operators */
%token EQUAL
%token ASSIGN
%token LARROW
%token RARROW
%token PAR
%token PARL
%token CHOICE
%token PIPE

/* Arithmetic operators */
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD

/* Logical operators */
%token OR
%token AND
%token NOT

/* Relations */
%token LT
%token GT
%token LEQ
%token GEQ
%token EQ
%token NEQ

/* Built-in functions */
%token FST
%token SND
%token RAND
%token SHOW
%token CONS
%token CONCAT
%token LOOKUP
%token LENGTH
%token MEM
%token UNION
%token PRINT
%token REV

/* Types */
/*%token TYINT
%token TYBOOL
%token TYSTRING*/

/* Punctuation */
%token DOT
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE
%token COMMA
%token SEMI
%token USCORE
/*%token COLON*/
%token RRARROW
%token EOF

/* Precedence and assoc */
%nonassoc NU_PREC
%right PAR PARL CHOICE
%nonassoc IN_PREC
%nonassoc DOT
%left SEMI
%nonassoc ASSIGN_PREC
%nonassoc THEN
%nonassoc ELSE
%right CONS CONCAT
%nonassoc SHOW
%nonassoc LENGTH
%nonassoc OR
%nonassoc AND
%nonassoc NOT
%nonassoc LT GT LEQ GEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD

%start file
%type <Syntax.process list> file
%start toplevel
%type <Syntax.process list> toplevel

%%

/* Grammar */

file:
  | EOF
    { [] }
  | e = expr EOF
    { [Process e] }
  | e = expr SEMI SEMI lst = file
    { Process e :: lst }

toplevel:
  | e = expr EOF
    { [Process e] }

/* PATTERNS --- The LHS of a case arm. */

pat_list:
  | p = pat_atom
    { [p] }
  | p = pat_atom COMMA ps = pat_list
    { p :: ps }

pat_atom:
  | x = NAME
    { PatName x }
  | x = IMPNAME
    { PatImpName x }
  | USCORE
    { PatWildcard }
  | UNIT
    { PatUnit }
  | t = TAG
    { PatTag t }
  | n = INT
    { PatInt n }
  | s = STRING
    { PatString s }
  | TRUE
    { PatBool true }
  | FALSE
    { PatBool false }
  | LBRACK RBRACK
    { PatList [] }
  | LBRACK p = pat_list RBRACK
    { PatList p }
/*  | p1 = pat CONS p2 = pat
    { PatCons (p1, p2) }*/
  | LPAREN p1 = pat_atom COMMA p2 = pat_list RPAREN
    { PatTuple (p1::p2) }
  | LPAREN p = pat_atom RPAREN
    { p }

expr:
  | e = atom_expr
    { e }
  | e = arith_expr
    { e }
  | e = bool_expr
    { e }
  | e = app_expr
    { e }
  | e = comm_expr
    { e }
  | e = proc_expr
    { e }
  | LAM xs = arg_list DOT e = expr
    { List.fold_right curry_lambdas xs e }
  | LET xs = pat_list EQUAL e1 = expr_list IN e2 = expr %prec IN_PREC
    { List.fold_left desugar_let e2 (List.rev (List.combine xs e1)) }
/*  | LET xs = var_ty_list EQUAL e1 = expr_list IN e2 = expr %prec IN_PREC
    { List.fold_left desugar_let e2 (List.rev (List.combine xs e1)) } */
  | LETREC x = NAME EQUAL e1 = expr IN e2 = expr %prec IN_PREC
    { LetRec (x, e1, e2) }
  | LET p = pat_atom ASSIGN e = expr %prec ASSIGN_PREC
    { Assign (p, e) }
/*  | LET x = expr COLON t = ty ASSIGN e = expr %prec ASSIGN_PREC
    { Assign (x, e) }*/
  | MATCH e1 = expr WITH p = pat_atom IN e3 = expr %prec IN_PREC
    { Let (p, e1, e3) }
  | MATCH e1 = expr WITH bs = branches
    { Match (e1, bs) }
  | IF b = expr THEN e1 = expr
    { IfT (b, e1) }
  | IF b = expr THEN e1 = expr ELSE e2 = expr
    { IfTE (b, e1, e2) }
/* Matt says: "What's REQ?" */
  | REQ e1 = expr IN e2 = expr %prec IN_PREC
    { Req (e1, e2) }
  | e1 = expr SEMI e2 = expr
    { Seq (e1, e2) }
   
atom_expr:
  | x = NAME
    { Name x }
  | x = IMPNAME
    { ImpName x }
  | USCORE
    { Wildcard }
  | UNIT
    { Unit }
  | t = TAG
    { Tag t }
  | n = INT
    { Int n }
  | s = STRING
    { String s }
  | TRUE
    { Bool true }
  | FALSE
    { Bool false }
  | LBRACK RBRACK
    { List [] }
  | LBRACK e = expr_list RBRACK
    { List e }
  | LBRACE RBRACE
    { Set [] }
  | LBRACE e = expr_list RBRACE
    { Set e }
  | LPAREN e1 = expr COMMA e2 = expr_list RPAREN
    { Tuple (e1::e2) }
  | RAND UNIT
    { Rand }
  | LPAREN e = expr RPAREN
    { e }
    
arith_expr:
  | e1 = expr PLUS e2 = expr
    { Plus (e1, e2) }
  | e1 = expr MINUS e2 = expr
    { Minus (e1, e2) }
  | e1 = expr TIMES e2 = expr
    { Times (e1, e2) }
  | e1 = expr DIVIDE e2 = expr
    { Divide (e1, e2) }
  | e1 = expr MOD e2 = expr
    { Mod (e1, e2) }

bool_expr:
  | e1 = expr LT e2 = expr
    { Lt (e1, e2) }
  | e1 = expr GT e2 = expr
    { Gt (e1, e2) }
  | e1 = expr LEQ e2 = expr
    { Leq (e1, e2) }
  | e1 = expr GEQ e2 = expr
    { Geq (e1, e2) }
  | e1 = expr OR e2 = expr
    { Or (e1, e2) }
  | e1 = expr AND e2 = expr
    { And (e1, e2) }
  | NOT e1 = expr
    { Not e1 }
  | e1 = expr EQ e2 = expr
    { Eq (e1, e2) }
  | e1 = expr NEQ e2 = expr
    { Neq (e1, e2) }

app_expr:
  | x = NAME es = atom_list
    { List.fold_left curry (Name x) es }
  | LPAREN x = expr RPAREN es = atom_list
    { List.fold_left curry x es }
  | THUNK e = atom_expr
    { Thunk e }
  | FORCE e = atom_expr
    { Force e }
  | FST e = atom_expr
    { Fst e }
  | SND e = atom_expr
    { Snd e }
  | SHOW e = expr
    { Show e }
  | e1 = expr CONS e2 = expr
    { Cons (e1, e2) }
  | e1 = expr CONCAT e2 = expr
    { Concat (e1, e2) }
  | LOOKUP e1 = atom_expr e2 = atom_expr
    { Lookup (e1, e2) }
  | LENGTH e = expr
    { Length e }
  | MEM e1 = atom_expr e2 = atom_expr
    { Mem (e1, e2) }
  | UNION e1 = atom_expr e2 = atom_expr
    { Union (e1, e2) }
  | PRINT e = atom_expr
    { Print e }
  | REV e = atom_expr
    { Rev e }

comm_expr:
  | WR e = expr RARROW c = NAME
    { Wr (e, c) }
  | WR e = expr RARROW c = IMPNAME
    { Wr (e, c) }
  | RD x = NAME LARROW c = NAME
    { RdBind (x, c) }
  | RD x = NAME LARROW c = IMPNAME
    { RdBind (x, c) }
  | RD c = NAME
    { Rd c }
  | RD c = IMPNAME
    { Rd c }
  | NU xs = arg_list DOT e = expr %prec NU_PREC
    { let names = List.map get_names xs in
      Nu (names, e) }

proc_expr:
  | e1 = expr PAR e2 = expr
    { ParComp (e1, e2) }
  | e1 = expr PARL e2 = expr
    { ParLeft (e1, e2) }
  | e1 = expr CHOICE e2 = expr
    { Choice (e1, e2) }

expr_list:
  | e = expr
    { [e] }
  | e1 = expr COMMA e2 = expr_list
    { e1 :: e2 }

/*var_ty_list:
  | e = expr COLON ty
    { [e] }
  | e1 = expr COLON ty COMMA e2 = var_ty_list
    { e1 :: e2 }*/

arg_list:
  | e = NAME
    { [Name e] }
  | UNIT
    { [Unit] }
  | e1 = NAME COMMA e2 = arg_list
    { Name e1 :: e2 }
  | UNIT COMMA e2 = arg_list
    { Unit :: e2 }

atom_list:
  | e = atom_expr
    { [e] }
  | e1 = atom_expr e2 = atom_list
    { e1 :: e2 }

/* Matt says: The LHS of a match should be a _pattern_, not an expression */
branches:
  | PIPE p = pat_atom RRARROW e = expr END
    { [(p,e)] }
  | PIPE p = pat_atom RRARROW e = expr bs = branches
    { (p,e) :: bs }

/*atom_ty :
  | TYINT
    { TyInt }
  | TYBOOL
    { TyBool }
  | TYSTRING
    { TyString }
  | LPAREN t = atom_ty RPAREN
    { t }

ty :
  | t1 = atom_ty RARROW t2 = ty
    { TyArrow (t1, t2) }
  | t = atom_ty
    { t }
*/