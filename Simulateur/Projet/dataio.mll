{
open Netlist_ast
open Def
open Lexing
exception Lexing_error of string
let v = ref [||]
let l = ref []
let word_size = ref 0 

let tonewline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}


let chiffre = [ '0'-'9']
let bin = "0" | "1"
let space = ' ' | '\t' | '\r'
let nombre = chiffre+
let newline = '\n' | '\r'

rule header = parse
   |(nombre as ssize) space+ (nombre as sword_size) space* newline 
     {
	  if (int_of_string ssize <= 0) then raise(Lexing_error "Header invalide");
	  v := Array.make (int_of_string ssize) (VBitArray (Array.make !word_size false)) ; 
      if (!word_size <> int_of_string sword_size) then raise (Lexing_error ("unexpected word size :"^sword_size)) ; 
      }
   |_ {raise(Lexing_error "Header invalide")}
   
and ram_cases = parse
	| (nombre as n1) space* "-" space*(nombre as n2) space * ";" space* {let nb1 = int_of_string n1 and nb2 = int_of_string n2 in 
						l:= (!l)@(Array.to_list (Array.init (nb2 - nb1 + 1) (fun i -> nb1 +i))) ; ram_cases lexbuf}
	| (nombre as n) space* ';'  {let nb = int_of_string n in l:= (!l)@[nb] ; ram_cases lexbuf}
	| space* {ram_cases lexbuf}
	| newline {}
	| eof | _  {raise(Lexing_error "declaration des cases a imprimer invalide") }
   
and contenu i = parse
	| bin* as b {	
		(*if (i >= Array.length (!v)) then raise (Lexing_error "too much words") ;*)
		if (String.length b <> !word_size) then raise (Lexing_error "invalid word") ;
		(!v).(i) <- VBitArray (Array.init (String.length b) (fun j -> b.[j] = '1')) ;
		if (i+1 = Array.length (!v)) then ()
		else contenu (i+1) lexbuf
                }
	|newline {tonewline lexbuf; contenu i lexbuf}
	|space {contenu i lexbuf}
	| eof {if (i < Array.length !v) then raise (Lexing_error "missing words")}
	| _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }             

{
let read_ram options = 
	let lexbuf = Lexing.from_channel (open_in options.ram_file) in
	try
	word_size := 8 ;
	header lexbuf ;
	ram_cases lexbuf ; 
	options.ramlist <- !l ;
	contenu 0 lexbuf ;
	!v ;
	with
	| Lexing_error s -> Format.eprintf "Erreur chargement ram ligne %d : %s @." lexbuf.lex_curr_p.pos_lnum s ; exit 1 
	
let read_rom options = 
	let lexbuf = Lexing.from_channel (open_in options.rom_file) in
	try
	word_size := 16 ;
	header lexbuf ;
	contenu 0 lexbuf ;
	!v ;
	with
	| Lexing_error s -> Format.eprintf "'%s' : Erreur chargement rom ligne %d : %s@." options.rom_file lexbuf.lex_curr_p.pos_lnum s ; exit 1 
	
	(*Remarque importante!! les fonctions ci dessus renverront une erreur si le nombre de lignes ne correspond pas exactement
	à la taille déclarée dans le header. Il faudra éventuellement compléter avec des lignes de 0 en fin de fichier... *)
}
