open List
open String
open LOLCODEast

type env = (ident * value) list
type funtable = funkshun list

exception UnboundVar of string
exception UndeclaredFun of string
exception InvalidCast of string
exception UndefinedMath of string
exception InvalidArgs of string
exception FoundYr of value
exception GetTheFout of env * funtable * value (*Hack for loops and functions*)


let ( ^^ ) (b1:bool) (b2:bool) : bool = (* logical xor *)
    if b1 then
        if b2 then false else true
    else
        if b2 then true else false

(*Environment functions*)

let new_env = []

let rec binds (id:ident) (rho:env) : bool =
    match rho with
          [] -> false
        | (a,b)::t -> if a = id then true else binds id t
 
let rec assign (id:ident) (v:value) (rho:env) : env =
    match rho with
          [] -> raise (UnboundVar ("U TOLD ME TO ASSINE TO VAR "^id^
                                " BUT ITZ NOT THERE"))
        | (a,b)::t -> if a = id then (a,v)::t else (a,b)::(assign id v t)

let extend (id:ident) (newval:value) (rho:env) : env =
    if (binds id rho) then
        (assign id newval rho)
    else
        (id,newval)::rho

let rec getval (id:ident) (rho:env) : value =
    match rho with
          [] -> raise (UnboundVar ("U TRIED TO USE VAR "^id^
                        " BUT I COULDNT FINDED IT."))
        | (a,b)::t -> if a = id then b else getval id t

let makeenv (identlis:ident list) (vallis:value list) : env =
    combine identlis vallis

(*function table functions*)

let rec exists (id:ident) (funs:funtable) : bool =
    match funs with
          [] -> false
        | (name,args,body)::t -> if name = id then true else (exists id t)

let rec redef ((funname, funargs, funbody):funkshun) (funs:funtable) :funtable =
    match funs with
          [] -> raise (UndeclaredFun("I MADE A ERRER: I TRIED TO REDEF A"^
                                        "FUNCION THAT WASNT THERE"))
        | (name,args,body)::t -> 
            if  name = funname then
                (funname, funargs, funbody)::t
            else
                (name, args, body)::(redef (funname, funargs, funbody) t)

let  addfunc (((name,args,body) as func):funkshun) (funs:funtable) : funtable =
    if exists name funs then
        redef func funs
    else
        func::funs

let rec getfun (id:ident) (funs:funtable) : funkshun =
    match funs with
          [] -> raise (UndeclaredFun("U NEVER TOLD ME HOW TO "^id^" SO I CANT"))
        | (name, args, body)::t -> 
            if name = id then 
                (name, args, body)
            else 
                getfun id t

(*Internal auxilliary functions*)

let coax_to_troof (v:value) : value =
    match v with
          NUMBR i -> if i=0 then FAIL else WIN
        | NUMBAR f -> if f=0.0 then FAIL else WIN
        | WIN -> WIN
        | FAIL -> FAIL
        | YARN s -> if s="" then FAIL else WIN
        | NOOB -> FAIL

let coax_to_bool (v:value) : bool =
    match v with
          NUMBR i -> not ( i=0 )
        | NUMBAR f -> not (f=0.0)
        | WIN -> true
        | FAIL -> false
        | YARN s -> not (s="")
        | NOOB -> false

let coax_to_string (v:value) : string =
    match v with
          NUMBR i -> (string_of_int i)
        | NUMBAR f -> (string_of_float f)
        | WIN -> "WIN"
        | FAIL -> "FAIL"
        | YARN s -> s
        | NOOB -> ""

let coax_to_number  (v:value) : value =
    match v with
          NUMBR i -> NUMBR i
        | NUMBAR f -> NUMBAR f
        | WIN -> NUMBR(1)
        | FAIL -> NUMBR(0)
        | YARN s -> 
            if (contains s '.') then 
                NUMBAR(float_of_string s)
            else
                NUMBR(int_of_string s)
        | NOOB -> raise (InvalidCast "I CANT MAEK A NOOB A NUMBUR")
    

(*Interpretation*)

let rec evallis (elis:exp list) (rho:env) (funs:funtable) (it:value) : value list =
    match elis with
          [] -> []
        | h::t -> (eval h rho funs it)::(evallis t rho funs it)

and eval (e:exp) (rho:env) (funs:funtable) (it:value) : value = 
      match e with
          LITERAL lit -> lit
        | MAEK_A (id, label) -> 
            (match (getval id rho), label with
                  NUMBR i, NUMBR_L -> NUMBR(i)
                | NUMBR i, NUMBAR_L -> NUMBAR(float_of_int i)
                | NUMBR i, TROOF_L -> if i = 0 then FAIL else WIN
                | NUMBR i, YARN_L -> YARN(string_of_int i)
                | NUMBAR f, NUMBR_L -> NUMBR(int_of_float f)
                | NUMBAR f, NUMBAR_L -> NUMBAR(f)
                | NUMBAR f, TROOF_L -> if f = 0.0 then FAIL else WIN
                | NUMBAR f, YARN_L -> YARN(string_of_float f)
                | WIN, NUMBR_L -> NUMBR (1)
                | WIN, NUMBAR_L -> NUMBAR (1.0)
                | WIN, TROOF_L -> WIN
                | WIN, YARN_L -> YARN("WIN")
                | FAIL, NUMBR_L -> NUMBR (0)
                | FAIL, NUMBAR_L -> NUMBAR (0.0)
                | FAIL, TROOF_L -> FAIL
                | FAIL, YARN_L -> YARN("FAIL")
                | YARN s, NUMBR_L -> NUMBR( int_of_string s )
                | YARN s, NUMBAR_L -> NUMBAR( float_of_string s)
                | YARN s, TROOF_L -> if s = "" then FAIL else WIN
                | YARN s, YARN_L -> YARN(s)
                | NOOB, NUMBR_L -> NUMBR(0)
                | NOOB, NUMBAR_L -> NUMBAR(0.0)
                | NOOB, TROOF_L -> FAIL
                | NOOB, YARN_L -> YARN("")
                | _, NOOB_L -> raise (InvalidCast "U CANT MAEK A VAR A NOOB"))
        | VAR id ->  if id = "IT" then it else getval id rho
        | BOTH_SAEM (e1, e2) -> 
            (match (eval e1 rho funs it), (eval e2 rho funs it) with
                  NUMBR i1, NUMBR i2 -> if i1 = i2 then WIN else FAIL
                | NUMBAR f, NUMBR i -> if f=(float_of_int i) then WIN else FAIL
                | NUMBR i, NUMBAR f -> if (float_of_int i)=f then WIN else FAIL
                | NUMBAR f1, NUMBAR f2 -> if f1 = f2 then WIN else FAIL
                | WIN, WIN -> WIN
                | WIN, FAIL -> FAIL 
                | FAIL, WIN -> FAIL
                | FAIL, FAIL -> WIN
                | YARN s1, YARN s2 -> if s1=s2 then WIN else FAIL
                | NOOB, NOOB -> WIN
                | _, NOOB -> FAIL
                | NOOB, _ -> FAIL
                | _, _ -> FAIL)
        | DIFFRINT (e1, e2) -> 
            (match (eval e1 rho funs it), (eval e2 rho funs it) with
                  NUMBR i1, NUMBR i2 -> if i1 = i2 then FAIL else WIN
                | NUMBAR f, NUMBR i -> if f=(float_of_int i) then FAIL else WIN
                | NUMBR i, NUMBAR f -> if (float_of_int i)=f then FAIL else WIN
                | NUMBAR f1, NUMBAR f2 -> if f1 = f2 then FAIL else WIN
                | WIN, WIN -> FAIL
                | WIN, FAIL -> WIN 
                | FAIL, WIN -> WIN
                | FAIL, FAIL -> FAIL
                | YARN s1, YARN s2 -> if s1=s2 then FAIL else WIN
                | NOOB, NOOB -> FAIL
                | _, NOOB -> WIN
                | NOOB, _ -> WIN
                | _, _ -> WIN)
        | BOTH_OF (e1, e2) -> if (coax_to_bool (eval e1 rho funs it)) &
                                    (coax_to_bool (eval e2 rho funs it))
                              then WIN else FAIL
        | WON_OF (e1, e2) -> if (coax_to_bool (eval e1 rho funs it)) or
                                    (coax_to_bool (eval e2 rho funs it))
                             then WIN else FAIL
        | EITHER_OF (e1, e2) -> if (coax_to_bool (eval e1 rho funs it)) ^^
                                    (coax_to_bool (eval e2 rho funs it))
                                then WIN else FAIL
        | SUM_OF (e1, e2) -> 
            (match (coax_to_number (eval e1 rho funs it)), 
                  (coax_to_number (eval e2 rho funs it)) with
                  NUMBR i1, NUMBR i2 -> NUMBR(i1+i2)
                | NUMBR i, NUMBAR f -> NUMBAR( (float_of_int i) +. f)
                | NUMBAR f, NUMBR i -> NUMBAR( f +. (float_of_int i))
                | NUMBAR f1, NUMBAR f2 -> NUMBAR (f1 +. f2)
                | _ -> raise (InvalidArgs ("I CANT TAKE SUM OF THESE")))
        | DIFF_OF (e1, e2) ->
             (match (coax_to_number (eval e1 rho funs it)), 
                  (coax_to_number (eval e2 rho funs it)) with
                  NUMBR i1, NUMBR i2 -> NUMBR(i1-i2)
                | NUMBR i, NUMBAR f -> NUMBAR( (float_of_int i) -. f)
                | NUMBAR f, NUMBR i -> NUMBAR( f -. (float_of_int i))
                | NUMBAR f1, NUMBAR f2 -> NUMBAR (f1 -. f2))
        | PRODUKT_OF (e1, e2) ->
            (match (coax_to_number (eval e1 rho funs it)), 
                  (coax_to_number (eval e2 rho funs it)) with
                  NUMBR i1, NUMBR i2 -> NUMBR(i1*i2)
                | NUMBR i, NUMBAR f -> NUMBAR( (float_of_int i) *. f)
                | NUMBAR f, NUMBR i -> NUMBAR( f *. (float_of_int i))
                | NUMBAR f1, NUMBAR f2 -> NUMBAR (f1 *. f2))
    
        | QUOSHUNT_OF (e1, e2) ->             
            (match (coax_to_number (eval e1 rho funs it)), 
                  (coax_to_number (eval e2 rho funs it)) with
                  NUMBR i1, NUMBR i2 ->  
                    if i2 = 0 then raise
                     (UndefinedMath "UR NOT CHUCK NORRIS U CANT DIVIDE BY ZERO")
                    else
                        NUMBR(i1/i2) 
                | NUMBR i, NUMBAR f -> 
                    if f = 0.0 then raise
                     (UndefinedMath "UR NOT CHUCK NORRIS U CANT DIVIDE BY ZERO")
                    else
                        NUMBAR((float_of_int i)/.f)
                | NUMBAR f, NUMBR i -> 
                    if i = 0 then raise
                     (UndefinedMath "UR NOT CHUCK NORRIS U CANT DIVIDE BY ZERO")
                    else
                        NUMBAR(f/.(float_of_int i))
                | NUMBAR f1, NUMBAR f2 -> 
                    if f2 = 0.0 then raise
                     (UndefinedMath "UR NOT CHUCK NORRIS U CANT DIVIDE BY ZERO")
                    else
                        NUMBAR(f1/.f2))
        | MOD_OF (e1, e2) ->
            (match (coax_to_number (eval e1 rho funs it)), 
                  (coax_to_number (eval e2 rho funs it)) with
                  NUMBR i1, NUMBR i2 -> 
                    if i2 = 0 then
                        raise (UndefinedMath "MOD BY ZERO THAT UNPOSSIBLE")
                    else
                        NUMBR(i1 mod i2)
                | _, _ -> raise 
                (UndefinedMath "THESE THINGS RNT NUMBRS I CANT DO MOD"))
        | BIGGR_OF (e1, e2) ->
            (match (coax_to_number (eval e1 rho funs it)), 
                  (coax_to_number (eval e2 rho funs it)) with
                  NUMBR i1, NUMBR i2 -> if i1 > i2 then NUMBR(i1) else NUMBR(i2)
                | NUMBR i, NUMBAR f -> if (float_of_int i) > f then 
                                        NUMBAR(float_of_int i) else NUMBAR(f)
                | NUMBAR f, NUMBR i -> if f > (float_of_int i) then
                                        NUMBAR(f) else NUMBAR(float_of_int i)
                | NUMBAR f1, NUMBAR f2 -> if f1 > f2 then 
                                           NUMBAR(f1) else NUMBAR(f2))
        | SMALLR_OF (e1, e2) -> 
            (match (coax_to_number (eval e1 rho funs it)), 
                  (coax_to_number (eval e2 rho funs it)) with
                  NUMBR i1, NUMBR i2 -> if i1 < i2 then NUMBR(i1) else NUMBR(i2)
                | NUMBR i, NUMBAR f -> if (float_of_int i) < f then 
                                        NUMBAR(float_of_int i) else NUMBAR(f)
                | NUMBAR f, NUMBR i -> if f < (float_of_int i) then 
                                        NUMBAR(f) else NUMBAR(float_of_int i)
                | NUMBAR f1, NUMBAR f2 -> if f1 < f2 then 
                                            NUMBAR(f1) else NUMBAR(f2))
        | NOT e' -> if (coax_to_bool (eval e' rho funs it)) then FAIL else WIN
        | ANY_OF el -> 
            let rec listsearch elis = (match elis with
                  [] -> FAIL
                | h::t -> if (coax_to_bool (eval h rho funs it)) then WIN 
                          else listsearch t)
            in listsearch el
        | ALL_OF el ->
            let rec listsearch elis = (match elis with
                  [] -> WIN
                | h::t -> if (not (coax_to_bool (eval h rho funs it))) then FAIL 
                          else listsearch t)
            in listsearch el
        | SMOOSH el ->
            let rec concatonator elis = (match elis with
                  [] -> ""
                | h::t ->(coax_to_string (eval h rho funs it))^(concatonator t))
            in YARN(concatonator el)
        | FUNKSHUN_CALL (id, exlis) ->
            let arglis = evallis exlis rho funs it and
                ((funame, funargs, funbody) as func) = (getfun id funs) in
                    (try 
                       (execfun funbody (makeenv funargs arglis) funs NOOB)
                     with 
                        Invalid_argument _ -> raise 
                            (InvalidArgs ("U DIDNT GIVE THE RITE NUMBER OF"^
                                    " ARGS TO "^id)))

and execstmtlis (sl:stmt list) (rho:env) (funs:funtable) (it:value): env * funtable * value =
    match sl with
          [] -> (rho,funs,it)
        | h::t -> 
            let (newenv,newfuns,newit) = execstmt h rho funs it in
                execstmtlis t newenv newfuns newit

and execfun (funbody:stmt list) (rho:env) (funs:funtable) (it:value) : value =
    try 
        let (_,_,retval) = (execstmtlis funbody rho funs it) in
            retval (*Reached end of function block; implicit return*)
    with 
          FoundYr funval -> funval (*Explicit return*)
        | GetTheFout _ -> NOOB (*Return with no value*)

and execstmt (s:stmt) (rho:env) (funs:funtable) (it:value):(env*funtable*value)= 
    match s with
          ORLY (s1, s2) -> 
            if (coax_to_bool it) then
                (execstmtlis s1 rho funs it) 
            else
                (execstmtlis s2 rho funs it)
        | IS_NOW_A(id, typename) ->
           if (binds id rho) then
              let newval = (eval (MAEK_A(id,typename)) rho funs it) in
                  ((extend id newval rho),funs,it)
           else (*getval should throw this error, but in case it doesnt'*)
               raise (UnboundVar ("I TRIED TO CAST VAR "^id
                                ^", BUT I COULDNT FINDED IT"))
        | I_HAS_A(id, e) -> ((extend id (eval e rho funs it) rho),funs,it)
        | R (id,e) -> 
              if (binds id rho) then
                  ((assign id (eval e rho funs it) rho),funs,it)
              else(*Already handled in assign, but again here for completeness*)
                  raise (UnboundVar ("U TOLD ME 2 ASSIGN TO VAR "^id
                                       ^", BUT ITZ DUZNT EXIST"))
        | IM_IN_YR(id, sl, lexp, lvar) -> 
            (try
                let (newenv,newfuns,newit) = (execstmtlis sl rho funs it) in
                    execstmt s newenv newfuns newit
            with GetTheFout (rho', funs', it')->
                (rho', funs', it'))
        | FUNKSHUN_DEF ((name,args,body) as func) ->    
            (rho, (addfunc func funs), it)
        | EXP e -> (rho,funs,(eval e rho funs it))
        | FOUND_YR e -> raise (FoundYr (eval e rho funs it))
        | GTFO -> raise (GetTheFout (rho,funs,it))



