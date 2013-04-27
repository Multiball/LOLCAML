type program = PROGRAM of stmt list

and stmt = ORLY of stmt list * stmt list (*stmt is executed when true*)
    | IS_NOW_A of ident * typename
    | I_HAS_A of ident * exp
    | R of ident * exp
    | IM_IN_YR of ident * stmt list * loopexp * ident
    (*in the following two, exp is the looping condition*)
    | IM_IN_YR_TIL of ident * stmt list * loopexp * exp
    | IM_IN_YR_WILE of ident * stmt list * loopexp * exp
    | WTF of switchblock list
    | FUNKSHUN_DEF of ident * stmt list
    | EXP of exp
    | GTFO
    | FOUND_YR of exp

and exp = 
      MAEK_A of ident * typename
    | VAR of ident
    | LITERAL of value
    | BOTH_SAEM of exp * exp
    | DIFFRINT of exp* exp
    | BOTH_OF of exp * exp
    | WON_OF of exp * exp
    | EITHER_OF of exp * exp
    | SUM_OF of exp * exp
    | DIFF_OF of exp * exp
    | PRODUKT_OF of exp * exp
    | QUOSHUNT_OF of exp * exp
    | MOD_OF of exp * exp
    | BIGGR_OF of exp * exp
    | SMALLR_OF of exp * exp
    | NOT of exp
    | ANY_OF of exp list
    | ALL_OF of exp list
    | SMOOSH of exp list
    | FUNKSHUN_CALL of ident * exp list

and value = NUMBR of int
    | NUMBAR of float
    | WIN
    | FAIL
    | YARN of string
    | NOOB

and loopexp = UPPIN
    | NERFIN 
    | OTHER
    | NUFFIN

and typename = TROOF_L | NUMBR_L | NUMBAR_L | YARN_L | NOOB_L

and ident = string

and condblock = exp * stmt list (* serves as 'MEBBE' and  'NO WAI'*)

and switchblock = exp * stmt list



