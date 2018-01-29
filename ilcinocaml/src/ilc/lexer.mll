{
    open Parser
}

rule token = parse
    | "(*"                      { comment 1 lexbuf }
    (* Whitespace *)
    | [' ' '\t' '\r' '\n']      { token lexbuf }
    (* Operators *)
    | "->"                      { RARROW }
    | "<-"                      { LARROW }
    | '='                       { EQUAL }
    | '<'                       { LESS }
    | '+'                       { PLUS }
    | '-'                       { MINUS }
    | '*'                       { TIMES }
    | '/'                       { DIVIDE }
    | '%'                       { MOD }
    | '|'                       { PAR }
    | ".|"                      { PARL }
    (* Reserved words *)
    | "let"                     { LET }
    | "lam"                     { LAM }
    | "nu"                      { NU }
    | "wr"                      { WR }
    | "rd"                      { RD }
    | "in"                      { IN }
    | "if"                      { IF }
    | "then"                    { THEN }
    | "else"                    { ELSE }
    (* Punctuation *)
    | '.'                       { DOT }
    | '('                       { LPAREN }
    | ')'                       { RPAREN }
    (* Identifiers and literals *)
    | "true"                    { TRUE }
    | "false"                   { FALSE }
    | ['0'-'9']+                { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | ['a'-'z' 'A'-'Z']+ ['a'-'z' 'A'-'Z' '0'-'9' '_']* { NAME (Lexing.lexeme lexbuf) }
    | eof                       { EOF }
and comment depth = parse
    | "(*"                      { comment (depth + 1) lexbuf }
    | "*)"                      { if depth = 1
                                  then token lexbuf
                                  else comment (depth - 1) lexbuf }
    | _                         { comment depth lexbuf }
