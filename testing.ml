#load "LOLCODEast.cmo";;
#load "PARSELOL.cmo";;
#load "LEXLOL.cmo";;
#load "EXECLOL.cmo";;

open LOLCODEast

let maxbuf = 1024

let string_of_value (v:value) =
    match v with
          NUMBR i -> ("NUMBR("^(string_of_int i)^")")
        | NUMBAR f -> "NUMBAR("^(string_of_float f)^")"
        | WIN -> "TROOF(WIN)"
        | FAIL -> "TROOF(FAIL)"
        | YARN s -> "YARN(\""^s^"\")"
        | NOOB -> "NOOB"

let rec string_of_env (rho:(ident*value)list) = 
    match rho with
          [] -> "\n"
        | (a,b)::t -> "("^a^","^(string_of_value b)^")"^(string_of_env t)

let print_env rho = print_string (string_of_env rho)

let lex_and_parse s = PARSELOL.program LEXLOL.tokenize (Lexing.from_string s)


let run prog = match prog with
      PROGRAM progl -> 
        let result = (EXECLOL.execstmtlis progl [] NOOB) in 
            print_string ("Environoment:"^(string_of_env (fst result))^"|IT:"^(string_of_value (snd result))^"\n")            

let exec_file  =
    (try
        let file = open_in (read_line ()) in
            let prog = PARSELOL.program LEXLOL.tokenize (Lexing.from_channel file) in
                match prog with
                    PROGRAM progl ->
                        let result = (EXECLOL.execstmtlis progl [] NOOB) in
                            print_string ("Environoment:"^(string_of_env (fst result))^"|IT:"^(string_of_value (snd result))^"\n")
   with Sys_error _ -> print_string ("Unable to open file") )

