%token <int> INT
%token <string> ID
%token DEFUN
%token DEFCONST
%token LET
%token CONS
%token CAR
%token CDR
%token IF
%token NIL
%token T
%token LPAREN
%token RPAREN
%token GREATER_EQUAL
%token GREATER
%token LESS_EQUAL
%token LESS
%token EQUAL
%token NOT_EQUAL
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token ATOMP
%token QUOTE
%token LIST
%token RECUR
%token DEFUN_TAIL
%token BEGIN
%token LAMBDA
%token BREAK
%token EOF

%start <Glisp.defs option> prog
%%

prog:
    | EOF { None }
    | d = defs; EOF { Some d }
    ;

defs:
    | d = defun; ds = defs { d :: ds }
    | d = defconst; ds = defs { d :: ds }
    | { [] }
    ;

defun:
    | LPAREN DEFUN; fn=ID; env=envs; body=statements; RPAREN { Glisp.Defun (fn,env,body) }
    | LPAREN DEFUN_TAIL; fn=ID; env=envs; body=statements; RPAREN { Glisp.Defun_Tail (fn,env,body) }
    ;

defconst:
    | LPAREN DEFCONST; n=ID; v=INT; RPAREN { Glisp.Defconst (n,v) }
    ;

envs:
    | e = env; es = envs { e :: es }
    | { [] }
    ;

env:
    | e=ID { e }
    ;

statements:
    | s = statement; ss = statements { s :: ss }
    | { [] }
    ;

term_list:
    | t = term; ts = term_list { t :: ts }
    | { [] }
    ;

term:
    | LPAREN; ts=term_list; RPAREN { Glisp.TermQuote ts }
    | i=INT { Glisp.TermConstant i }
    | T { Glisp.TermConstant 1 }
    | NIL { Glisp.TermConstant 0}
    | s=ID { Glisp.TermSymbol s }
    ;
    
statement:
    | LPAREN; s=statement_body; RPAREN { s }
    | i=INT { Glisp.Constant i }
    | T { Glisp.Constant 1 }
    | NIL { Glisp.Constant 0}
    | s=ID { Glisp.Symbol s }
    | LPAREN LIST; body=statements; RPAREN { Glisp.List body }
    | QUOTE; LPAREN; body=term_list; RPAREN { Glisp.Quote body }
    ;

statement_body:
    | LET; LPAREN; env=let_envs; RPAREN; body=statements { Glisp.Let (env,body) }
    | BEGIN; LPAREN; s=statements; RPAREN {Glisp.Begin s }
    | IF; test=statement; true_statement=statement; false_statement=statement; { Glisp.If (test,true_statement,false_statement) }
    | fname=ID; ps=statements { Glisp.Function_Call (fname, ps) }
    | RECUR; fname=ID; ps=statements { Glisp.Tail_Function_Call (fname, ps) }
    | CONS; p1=statement; p2=statement { Glisp.Cons (p1,p2) }
    | CAR; s=statement { Glisp.Car s }
    | CDR; s=statement { Glisp.Cdr s }
    | BREAK; { Glisp.Break }
    | ATOMP; s=statement { Glisp.Atomp s }
    | GREATER_EQUAL; p1=statement; p2=statement { Glisp.Greater_Equal (p1,p2) }
    | GREATER; p1=statement; p2=statement { Glisp.Greater (p1,p2) }
    | LESS_EQUAL; p1=statement; p2=statement { Glisp.Less_Equal (p1,p2) }
    | LESS; p1=statement; p2=statement { Glisp.Less (p1,p2) }
    | EQUAL; p1=statement; p2=statement { Glisp.Equal (p1,p2) }
    | NOT_EQUAL; p1=statement; p2=statement { Glisp.Not_Equal (p1,p2) }
    | PLUS; p1=statement; p2=statement { Glisp.Plus (p1,p2) }
    | MINUS; p1=statement; p2=statement { Glisp.Minus (p1,p2) }
    | TIMES; p1=statement; p2=statement { Glisp.Times (p1,p2) }
    | DIVIDE; p1=statement; p2=statement { Glisp.Divide (p1,p2) }
    | LAMBDA; p=lambda_params; s=statements {Glisp.Lambda (p,s) }
    ;

lambda_params:
    | p=ID { [p] }
    | LPAREN; pl=lambda_param_list; RPAREN { pl }
    ;

lambda_param_list:
    | p=ID; pl=lambda_param_list { p::pl }
    | { [] }
    ;

let_envs:
    | l=let_env; ls=let_envs { l::ls }
    | { [] }
    ;

let_env:
    | LPAREN; name=ID; s=statement; RPAREN { (name,s) }
    ;
