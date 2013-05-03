{
open PARSELOL
open LOLCODEast
}

let LIT_NUMBR = '-' ? ['0'-'9'] +
let LIT_NUMBAR = '-' ? ['0'-'9'] + '.' ['0'-'9'] +
let YARN_CHARS = [^'\r' '\n' '\"']
let Identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let whitespace = [ ' ' '\t' ]
let endlines = ['\r' '\n' ','] | "\r\n"

rule tokenize = parse
    (* Literals *)
      (LIT_NUMBR as s)  {NUMBR_LIT(int_of_string s)}
    | LIT_NUMBAR as s  {NUMBAR_LIT(float_of_string s)}
    | '"' (YARN_CHARS* as s) '"'  {YARN_LIT(s)}
    | "WIN" {TROOF_LIT(true)}
    | "FAIL" {TROOF_LIT(false)}
    (*General keywords and control statements*)
    | "HAI" {HAI_T}
    | "KTHXBYE"{KTHXBYE_T}
    | "AN" {AN_T}
    | "YR" {YR_T}
    | "MAH" {MAH_T}
    | "ITZ" {ITZ_T}
    | "MKAY" {MKAY_T}
    | "NUMBR" {TYPENAME_T(NUMBR_L)}
    | "NUMBAR" {TYPENAME_T(NUMBAR_L)}
    | "TROOF" {TYPENAME_T(TROOF_L)}
    | "YARN" {TYPENAME_T(YARN_L)}
    | "NOOB" {TYPENAME_T(NOOB_L)}
    |  endlines* {LINEBREAK_T}
    | ("..." | "..." | "…" | "…") endlines {tokenize lexbuf (*ignore*)} (*Line continuation*)
    | "BTW" ([^ '\n' '\r'])* endlines {tokenize lexbuf (*one-line comment; ignore*)}
    | "OBTW" (_)* "TLDR" {tokenize lexbuf (*block comment; ignore*)}
    | whitespace {tokenize lexbuf (*ignore*)}
    (*Statement keywords*)
    | "I HAS A " {I_HAS_A_T}
    | "R" {R_T}
    | "O RLY?" {ORLY_T}
    | "YA RLY" {YA_RLY_T}
    | "NO WAI" {NO_WAI_T}
    | "OIC" {OIC_T}
    | "IM IN YR" {IM_IN_YR_T}
    | "IM OUTTA YR" {IM_OUTTA_YR_T}
    | "GTFO" {GTFO_T}
    | "HOW DUZ I" {HOW_DUZ_I_T}
    | "IF U SAY SO" {IF_U_SAY_SO_T}
    | "FOUND YR" {FOUND_YR_T}
    (*Expression keywords*)
    | "MAEK" {MAEK_T}
    | "A" {A_T}
    | "NOT" {NOT_T}
    | "BOTH SAEM" {BOTH_SAEM_T}
    | "DIFFRINT" {DIFFRINT_T}
    | "BOTH OF" {BOTH_OF_T}
    | "WON OF" {WON_OF_T}
    | "EITHER_OF" {EITHER_OF_T}
    | "ANY OF" {ANY_OF_T}
    | "ALL OF" {ALL_OF_T}
    | "SMOOSH" {SMOOSH_T}
    | "SUM OF" {SUM_OF_T}
    | "DIFF OF" {DIFF_OF_T}
    | "PRODUKT OF" {PRODUKT_OF_T}
    | "QUOSHUNT OF" {QUOSHUNT_OF_T}
    | "MOD OF" {MOD_OF_T}
    | "BIGGR OF" {BIGGR_OF_T}
    | "SMALLR OF" {SMALLR_OF_T}
    (*Match identifiers only after reserved keywords have been matched*)
    | Identifier as s {IDENT_T (s)}
    |eof {EOF_T}


