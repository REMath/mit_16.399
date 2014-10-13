/* File calculatorYACC.mly */

%{ (* header *)
  
type symbTable = (string * int) list ;;

let sb = ref([] : symbTable) ;;

let getvalue x =
   if (List.mem_assoc x !sb) then 
     (List.assoc x !sb)
   else
     0;;

let rec except x l = match l with
  []   -> []
| h::t -> if (h = x) then t
            else h::(except x t)

let setvalue x v =
  (print_string (x ^ " = "); print_int (v);
   print_string ";\n"; flush stdout;
   if (List.mem_assoc x !sb) then
     sb := (x, v) :: (except (x, (List.assoc x !sb)) !sb)
   else
     sb := (x, v) :: !sb 
  );;

%} /* declarations */

%token EOL SEMICOLON ASSIGN PLUS /* lexer tokens */
%token MINUS TIMES DIV LPAREN RPAREN
%token < string > IDENT
%token < int > NUM
%start prog                   /* the entry point */
%type <unit> prog  
%type <int> list
%type <int> cmd
%type <int> assign
%type <int> expr
%left PLUS MINUS            /* lowest precedence */
%left TIMES DIV             /* medium precedence */
%nonassoc UMINUS           /* highest precedence */

%% /* rules */

prog :
    list EOL  { print_int $1 ; print_newline(); flush stdout; () }
        
list :
    cmd SEMICOLON list  { $3 }
  | cmd                 { $1 }
  
cmd :
    assign  { $1 }
  | expr    { $1 }
  
assign :
    IDENT ASSIGN expr  { (setvalue $1 $3) ; $3 }
        
expr :
    expr PLUS expr           { $1 + $3 }
  | expr MINUS expr          { $1 - $3 }
  | expr TIMES expr          { $1 * $3 }
  | expr DIV expr            { $1 / $3 }
  | MINUS expr %prec UMINUS  { - $2 }
  | LPAREN expr RPAREN       { $2 }
  | IDENT                    { (getvalue $1) }
  | NUM                      { $1 }
  
%% (* trailer *)
