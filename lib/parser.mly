%token <string> NAME
%token LPAREN RPAREN EOF
%token WHITESPACE
%start program
%type <Telelisp.body list> program
%type <Telelisp.body> main

%%

nl: WHITESPACE {}
  | WHITESPACE nl {}
  ;

program:
  EOF { [] }
  | sexp nl program { $1 :: $3 }
  ;

main:
  sexp { $1 }
  ;

sexp:
  l = list { l }
| a = atom { a }
;

list:
  LPAREN RPAREN { Telelisp.Nil }
| LPAREN i = inside_list RPAREN { i }

inside_list:
  | sexp { Telelisp.Cons($1, Telelisp.Nil) } 
  | sexp inside_list { Telelisp.Cons($1, $2) }
;

atom: a = NAME { Telelisp.Atom a }
;