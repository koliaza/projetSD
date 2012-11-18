(* 
Ci-dessous exemple de code utilisé par le prof pour lire un fichier et le parser,
on peut s'en inspirer pour faire le dataio. 
Specs : prend l'argument options, et surtout les champs suivants :
ramfile, romfile, ramsize, romsize. 
les size sont en nombre de mots 8 bits qui sont comptés comme des VbitArray dans le format final.
les deux fonctions doivent rendre un VbitArray Array.





 let find_file filename =
  try
    open_in filename
  with
    | _ -> raise (Parse_error "No such file '%s'")

let read_file filename =
  let ic = find_file filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    Netlist_parser.program Netlist_lexer.token lexbuf
  with
    | e -> raise (Parse_error ("Syntax error (exception: "^(Printexc.to_string e)^")"))


*)
open Netlist_ast
open Def

let read_ram options = 
Array.make options.ramsize [|false ; false ; false ; false ; 
										false ; false ; false ; false |] 
										
let read_rom options = 
Array.make options.romsize  [|false ; false ; false ; false ; 
										false ; false ; false ; false |] 