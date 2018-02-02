%{
    open Syntax

    let rec flatten acc = function
        | ListItems (e1, e2) -> flatten (e1 :: acc) e2
        | e -> List.rev (e :: acc)
%}

/* Lexemes */
/* Identifiers and literals */
%token <int> INT
%token <Syntax.name> NAME
%token <string> STRING 
%token TRUE FALSE
/* Operators */
%token LARROW
%token RARROW
%token EQUAL
%token LT
%token GT
%token LEQ
%token GEQ
%token OR
%token AND
%token NOT
%token EQ
%token NEQ
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token PAR
%token PARL
%token CHOICE
/* Reserved */
%token LET
%token LAM
%token NU
%token WR
%token RD
%token IN
%token IF THEN ELSE
%token THUNK FORCE
/* Punctuation */
%token DOT
%token LPAREN RPAREN
%token LBRACK RBRACK
%token COMMA
%token EOF

/* Precedence and assoc */
%right PAR PARL CHOICE
%left DOT IN
%nonassoc ELSE
%right COMMA
%nonassoc OR
%nonassoc AND
%nonassoc LT GT LEQ GEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIVIDE
%left MOD

%start file
%type <Syntax.process list> file
%start toplevel
%type <Syntax.process> toplevel

%%

/* Grammar */

file:
    | EOF
      { [] }
    | e = expr EOF
      { [Process e] }

toplevel:
    | e = expr EOF
      { Process e }

expr:
    | x = NAME
      { Name x }
    | n = INT
      { Int n }
    | s = STRING
      { String s }
    | TRUE
      { Bool true }
    | FALSE
      { Bool false }
    /* Arithmetic */
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
    /* Comparison */
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
    /* Conditionals */
    | IF b = expr THEN e1 = expr ELSE e2 = expr
      { If (b, e1, e2) }
    /* Laziness */
    | THUNK LPAREN e = expr RPAREN
      { Thunk e }
    | FORCE e = expr
      { Force e }
    /* Let */
    | LET x = NAME EQUAL e1 = expr IN e2 = expr
      { Let (x, e1, e2) }
    /* App */
    | e1 = expr e2 = expr
      { App (e1, e2) }
    /* Lam */
    | LAM x = NAME DOT e = expr
      { Lam (x, e) }
    /* Wr */
    | WR e = expr RARROW x = NAME
      { Wr (e, x) }
    | RD x1 = NAME LARROW x2 = NAME
      { Rd (x1, x2) }
    | NU x = NAME DOT e = expr
      { Nu (x, e) }
    | e1 = expr PAR e2 = expr
      { ParComp (e1, e2) }
    | e1 = expr PARL e2 = expr
      { ParLeft (e1, e2) }
    | e1 = expr CHOICE e2 = expr
      { Choice (e1, e2) }
    | LPAREN e = expr RPAREN
      { e }
    | e1 = expr DOT e2 = expr
      { Seq (e1, e2) }
    | e1 = expr COMMA e2 = expr
      { ListItems (e1, e2) }
    | LBRACK e = expr RBRACK
      { List (flatten [] e) }
