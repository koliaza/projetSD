{

exception Lexing_error of string
let size = ref 0
let numbersize = ref 0
let str_en_construction = ref ""
let length_str_en_construction = ref 0
let tab_res = ref ([|0|])
let deja_parcouru = ref 0
}


let chiffre = [ '0'-'9']
let bin = "0" | "1"
let space = ' ' | '\t' | '\r'
let nombre = chiffre+
let newline = '\n' | '\r' '\n'

rule header = parse
   |nombre as ssize space+ nombre as snumbersize space* newline 
     {size:= int_of_string ssize; 
      if (!size <= 0) then raise(Lexing_error "Header invalide");
      numbersize := int_of_string snumbersize;
      if(!numbersize <> 8 or !numbersize <> 16) then raise(Lexing_error "The size of the numbers must be 8 or 16");
      contenu lexbuf; }
   |_ {raise(Lexing_error "Header invalide")}
   
and contenu =     
   |space | newline { contenu lexbuf }
   |bin as b {str_en_construction := String.concat !str_en_construction [String.make 1 b];
              length_str_en_construction:= !length_str_en_construction +1;    
              if (!length_str_en_construction = !numbersize) then 
                begin
                  length_str_en_construction := 0;
                  tab_res(!deja_parcouru ) <- int_of_string ("0b"^!str_en_construction);
                  str_en_construction := "";
                  deja_parcouru := !deja_parcouru+1;
                  if (!deja_parcouru = !size) 
                     then (!size,!numbersize,!tab_res)
                  else contenu lexbuf;
               end
              else contenu lexbuf;
                }
   | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }             






{



}
