%{
open LOLCODEast
open Parsing
exception LoopMismatch of string
exception ReservedWord of string
%}

%token <int> NUMBR_LIT
%token <float> NUMBAR_LIT
%token <bool> TROOF_LIT
%token <string> YARN_LIT
%token <LOLCODEast.ident> IDENT_T 
%token <LOLCODEast.typename> TYPENAME_T
%token LINEBREAK_T AN_T YR_T HAI_T KTHXBYE_T EOF_T A_T MAH_T

    /* expression-related*/
    NOT_T UPPIN_T NERFIN_T 
    ANY_OF_T ALL_OF_T SMOOSH_T MKAY_T 
    BOTH_SAEM_T DIFFRINT_T BOTH_OF_T WON_OF_T EITHER_OF_T
    SUM_OF_T DIFF_OF_T PRODUKT_OF_T QUOSHUNT_OF_T MOD_OF_T
    BIGGR_OF_T SMALLR_OF_T

    /* statement-related*/
    I_HAS_A_T R_T ITZ_T IS_NOW_A_T 
    HOW_DUZ_I_T IF_U_SAY_SO_T MAEK_T

    /* control flow-related */
    IM_IN_YR_T IM_OUTTA_YR_T TIL_T WILE_T ORLY_T YA_RLY_T NO_WAI_T
    MEBBE_T OIC_T WTF_T OMG_T OMGWTF_T GTFO_T 


%start program
%type <LOLCODEast.program> program


%nonassoc IDENT_T
%nonassoc LINEBREAK_T
%%

program:
   HAI_T LINEBREAK_T statements KTHXBYE_T LINEBREAK_T {PROGRAM $3}

statements:
      statement {[$1]}
    | LINEBREAK_T {[]}
    | statements statement { $1 @ [$2]}

expressions:
      expression {[$1]}
    | expressions expression {$1 @ [$2]}

statement:
      IDENT_T R_T expression LINEBREAK_T {if $1 = "IT" then raise (ReservedWord "ERRER: U CANT ASSINE TO 'IT' WITH 'R'") else R ($1, $3)}
    | IDENT_T IS_NOW_A_T TYPENAME_T LINEBREAK_T {IS_NOW_A( $1, $3)}
    | I_HAS_A_T IDENT_T ITZ_T expression LINEBREAK_T {if $2 = "IT" then raise (ReservedWord ("ERRER: U CANT MAEK A VAR NAMD 'IT'")) else I_HAS_A ($2, $4)}
    | I_HAS_A_T IDENT_T LINEBREAK_T { if $2 = "IT" then raise (ReservedWord ("ERRER: U CANT MAEK A VAR NAMD 'IT'")) else I_HAS_A($2, LITERAL(NOOB))}
    /* TODO: Add switch statement
    | WTF_T LINEBREAK_T caseblock OIC_T LINEBREAK_T {WTF $3}
    */
    | loop {$1}
    | GTFO_T LINEBREAK_T {GTFO}
    /*Currently only if-else. TODO: add elseif*/
    | ORLY_T LINEBREAK_T YA_RLY_T statements OIC_T LINEBREAK_T {ORLY ($4, [])}
    | ORLY_T LINEBREAK_T YA_RLY_T statements NO_WAI_T LINEBREAK_T statements OIC_T LINEBREAK_T { ORLY ($4,$7)}
    /* TODO: Implement functions
    | HOW_DUZ_I_T IDENT_T arglist LINEBREAK_T funstatements IF_U_SAY_SO_T LINEBREAK_T {FUNKSHUN_DEF ($2, $3, $4)}
    */
    | expression LINEBREAK_T {EXP($1)}


/* Currently only simple infinite loops. TODO: Implement loops with conditions and functions.*/
loop:
      IM_IN_YR_T IDENT_T LINEBREAK_T statements IM_OUTTA_YR_T IDENT_T LINEBREAK_T { if $2 = $6 then IM_IN_YR ($2, $4, NUFFIN, "") else raise (LoopMismatch ("ERRER: YR LOOP LBLS DONT MATCH ("^$2^" IZNT "^$6^")"))}
    /* 
    | IM_IN_YR_T IDENT_T loopop YR_T IDENT_T LINEBREAK_T loopstatements IN_OUTTA_YR_T IDENT_T LINEBREAK_T { if $2 != $9 then raise LoopMismatch "ERRER: YR LOOP LBLS DONT MATCH ("^$2^" IZNT "^$$9^")" $IM_IN_YR ($2, $4, $3) }
    | IM_IN_YR_T IDENT_T loopop YR_T IDENT_T TIL_T expression LINEBREAK_T loopstatements IM_OUTTA_YR_T IDENT_T LINEBREAK_T {if $2 != $11 then raise LoopMismatch "ERRER: YR LOOP LBLS DONT MATCH ("^$2^" IZNT "^$11^")" $IM_IN_YR ($2, $4, NOOB)}
    */


expression:
      NOT_T expression {NOT $2}
    | ANY_OF_T infop_args {ANY_OF $2}
    | ALL_OF_T infop_args {ALL_OF $2}
    | SMOOSH_T expressions MKAY_T {SMOOSH $2}
    | IDENT_T MAH_T infop_args {FUNKSHUN_CALL ($1,$3)}
    | TROOF_LIT {if $1 = true then LITERAL(WIN) else LITERAL(FAIL)}
    | NUMBR_LIT {LITERAL(NUMBR $1)}
    | NUMBAR_LIT {LITERAL(NUMBAR $1)}
    | YARN_LIT {LITERAL(YARN $1)}
    | IDENT_T {VAR $1}
    | MAEK_T IDENT_T A_T TYPENAME_T {MAEK_A($2,$4)}
    | BOTH_SAEM_T expression AN_T expression {BOTH_SAEM ($2,$4)}
    | DIFFRINT_T expression AN_T expression {DIFFRINT ($2, $4)}
    | BOTH_OF_T expression AN_T expression {BOTH_OF ($2,$4)}
    | WON_OF_T expression AN_T expression {WON_OF ($2,$4)}
    | EITHER_OF_T expression AN_T expression {EITHER_OF ($2,$4)}
    | SUM_OF_T expression AN_T expression {SUM_OF ($2,$4)}
    | DIFF_OF_T expression AN_T expression {DIFF_OF ($2,$4)}
    | PRODUKT_OF_T expression AN_T expression {PRODUKT_OF ($2,$4)}
    | QUOSHUNT_OF_T expression AN_T expression {QUOSHUNT_OF ($2,$4)}
    | MOD_OF_T expression AN_T expression {MOD_OF ($2,$4)}
    | BIGGR_OF_T expression AN_T expression {BIGGR_OF ($2,$4)}
    | SMALLR_OF_T expression AN_T expression {SMALLR_OF ($2,$4)}

infop_args:
      expression AN_T infop_args{[$1] @ $3}
    | expression MKAY_T {[$1]}
