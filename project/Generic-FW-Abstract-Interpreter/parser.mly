/* file parser.mly, parsing & concrete to abstract syntax translation */

%{ (* header *)
(* make a sequence of commands from two commands or *)
(* subsequences with flattening of the subsequences *)
let makeseq l r = match l, r with
  | Concrete_To_Abstract_Syntax.C_SEQ h, 
    Concrete_To_Abstract_Syntax.C_SEQ t 
    -> Concrete_To_Abstract_Syntax.C_SEQ (h @ t)
  | h, Concrete_To_Abstract_Syntax.C_SEQ t   
    -> Concrete_To_Abstract_Syntax.C_SEQ (h :: t)
  | Concrete_To_Abstract_Syntax.C_SEQ h, t
    -> Concrete_To_Abstract_Syntax.C_SEQ (h @ [t])
  | h,t
    -> Concrete_To_Abstract_Syntax.C_SEQ [h; t]

%} /* declarations */

%token <string> T_NAT
%token T_LPAR T_RPAR
%token T_RANDOM T_PLUS T_MINUS T_TIMES T_DIV T_MOD
%token T_TRUE T_FALSE T_LT T_LEQ T_EQ T_NEQ T_GT T_GEQ T_OR T_AND T_NEG
%token T_SKIP T_ASSIGN T_SEQ T_IF T_THEN T_ELSE T_FI T_WHILE T_DO T_OD
%token T_AINITIAL T_FINAL T_ALWAYS T_SOMETIME
%token <string> T_VAR
%token T_EOP

%start n_Prog /* grammar axiom non terminal */
%type <Abstract_Syntax.com> n_Prog /* program */
%type <Concrete_To_Abstract_Syntax.c_com>  n_Lco /* list of commands */
%type <Concrete_To_Abstract_Syntax.c_com>  n_Com /* command          */
%type <Concrete_To_Abstract_Syntax.c_bexp> n_Bexp /* boolean expr.   */
%type <Abstract_Syntax.aexp> n_Aexp /* arithmetic expression         */

%left     T_OR               /* lowest precedence */
%left     T_AND 
%right    T_NEG 
%nonassoc T_LT T_LEQ T_EQ T_NEQ T_GT T_GEQ 
%left     T_PLUS T_MINUS 
%left     T_TIMES T_DIV T_MOD 
%right    T_UPLUS T_UMINUS   /* highest precedence */

%% /* grammar rules */

n_Aexp:
  T_RANDOM                 { Abstract_Syntax.RANDOM              }
| T_NAT                    { (Abstract_Syntax.NAT $1)            }
| T_VAR { (Abstract_Syntax.VAR (Symbol_Table.add_symb_table $1)) }
| n_Aexp  T_PLUS   n_Aexp  { (Abstract_Syntax.PLUS  ($1, $3))    }
| n_Aexp  T_MINUS  n_Aexp  { (Abstract_Syntax.MINUS ($1, $3))    }
| n_Aexp  T_TIMES  n_Aexp  { (Abstract_Syntax.TIMES ($1, $3))    }
| n_Aexp  T_DIV    n_Aexp  { (Abstract_Syntax.DIV   ($1, $3))    }
| n_Aexp  T_MOD    n_Aexp  { (Abstract_Syntax.MOD   ($1, $3))    }
| T_PLUS  n_Aexp %prec T_UPLUS  { (Abstract_Syntax.UPLUS  $2)    }
| T_MINUS n_Aexp %prec T_UMINUS { (Abstract_Syntax.UMINUS $2)    }
| T_LPAR  n_Aexp  T_RPAR        { $2             }
;

n_Bexp:
  T_TRUE              { Concrete_To_Abstract_Syntax.C_TRUE           }
| T_FALSE             { Concrete_To_Abstract_Syntax.C_FALSE          }
| n_Aexp T_LT  n_Aexp { (Concrete_To_Abstract_Syntax.C_LT  ($1, $3)) }
| n_Aexp T_LEQ n_Aexp { (Concrete_To_Abstract_Syntax.C_LEQ ($1, $3)) }
| n_Aexp T_EQ  n_Aexp { (Concrete_To_Abstract_Syntax.C_EQ  ($1, $3)) }
| n_Aexp T_NEQ n_Aexp { (Concrete_To_Abstract_Syntax.C_NEQ ($1, $3)) }
| n_Aexp T_GT  n_Aexp { (Concrete_To_Abstract_Syntax.C_GT  ($1, $3)) }
| n_Aexp T_GEQ n_Aexp { (Concrete_To_Abstract_Syntax.C_GEQ ($1, $3)) }
| n_Bexp T_OR  n_Bexp { (Concrete_To_Abstract_Syntax.C_OR  ($1, $3)) }
| n_Bexp T_AND n_Bexp { (Concrete_To_Abstract_Syntax.C_AND ($1, $3)) }
| T_NEG n_Bexp        { (Concrete_To_Abstract_Syntax.C_NEG  $2)      }
| T_LPAR  n_Bexp T_RPAR { $2               }
;

n_Com:
  T_SKIP          { Concrete_To_Abstract_Syntax.C_SKIP               }
| T_VAR T_ASSIGN n_Aexp  
                  { (Concrete_To_Abstract_Syntax.C_ASSIGN 
                             ((Symbol_Table.add_symb_table $1), $3)) }
| T_IF n_Bexp T_THEN n_Lco  T_ELSE n_Lco T_FI 
                  { (Concrete_To_Abstract_Syntax.C_IF ($2, $4, $6))  }
| T_WHILE n_Bexp T_DO n_Lco T_OD  
                  { (Concrete_To_Abstract_Syntax.C_WHILE ($2, $4))   }
;

n_Lco:
     n_Com           { $1              }
| n_Com T_SEQ n_Lco  { (makeseq $1 $3) }
;

n_Prog:
     N_init n_Lco T_EOP  
              { (Concrete_To_Abstract_Syntax.label_normalize_com $2) }
;

N_init: 
     { Symbol_Table.init_symb_table () }

%%
(* trailer *)