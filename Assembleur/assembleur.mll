{
(*Remarque : 
non implémentés : subs, mults, jr*)

exception Assembleur_error of string 

let nombre_lignes = ref 0 

let key_words = Hashtbl.create 17 

let lkw = [
"add","10000000" ;
"adds","10000000" ;
"sub", "" ; (*non implémenté*)
"subs","10000001" ;
"mul","10100000" ;
(* "div","" *)
(*"divs","" ;*)
"jmp","00000100" ;
"jal","00011100" ;
"je","00011000" ;
"jl","00010100" ;
"jle","00010000" ;
"jm","00001100" ;
"jme","00001100" ;
"jne","00000000" ;
"jr","" ; (*non implementé*)
"test","00100000" ;
"tests","00100100" ;
"li", "01110000" ;
"lw", "01100000" ;
(*"la","" ;*)
"sw","01010000" ;
"and","11000000" ;
"not","11110000" ;
"or","11100000" ;
"xor","11010000" ;
"shl","11001000" ;
"shr","11101000" ;
"sla","110011000" ;
"sla","11011000" ;
"move","" (*non implementé*)
] 

let () = List.iter (fun (key,code) -> Hashtbl.add key_words key code) lkw 

let registres = Hashtbl.create 8 
let lreg = [
"ax","0000" ;
"bx","0010" ;
"cx","0100" ;
"dx","0110" ;
"ex","1000" ;
"sp","1010" ;
"ra","1110" ;
]
let () = List.iter (fun (reg,code) -> Hashtbl.add registres reg code ) lreg

let labels = Hashtbl.create 17 

let int_to_string i = 
	let s = String.create 8 in 
	for j = 0 to 7 do 
	  s.[j] <- if (i/(1 lsl j)) mod 2 = 0 then '0' else '1' ;
	done ;
	s
}

let key_w = (['a'-'z'] | ['A'-'Z'])+
let chiffre = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha|'_') (alpha | chiffre |'_')*
let sep = '\t' | ' ' | ','

rule get_lab i = parse 
 | sep* (ident as lab) sep* ":" sep* {if Hashtbl.mem labels lab 
			then (raise (Assembleur_error "label defined twice"))
			else Hashtbl.add labels lab (int_to_string i)}
 | '\n' {get_lab i lexbuf}
 | eof {}
 | "(*" {comment lexbuf ; get_lab i lexbuf}
 | [^'\n']* '\n' {get_lab (i+1) lexbuf}

and assembleur fft = parse 
 | sep* (key_w as op) sep+ '$' (ident as reg1) sep+ '$'(ident as reg2) sep* ('\n' | '\r')
	{
	Format.fprintf fft "%s%s%s@." (Hashtbl.find key_words op) (Hashtbl.find registres reg1) (Hashtbl.find registres reg2) ; decr nombre_lignes ;
	assembleur fft lexbuf}
 | sep* (key_w as op) sep+ (ident as lab) sep* ('\n'|'\r')
	{
	Format.fprintf fft "%s%s@." (Hashtbl.find key_words op) (Hashtbl.find labels lab); decr nombre_lignes ;
    	assembleur fft lexbuf}
 | sep *(key_w as op) sep+ (chiffre+ as i) sep* ('\n'|'\r') 
	{
	Format.fprintf fft "%s%s@." (Hashtbl.find key_words op) (int_to_string (int_of_string i)) ; decr nombre_lignes ;
	assembleur fft lexbuf } 
 | "(*" {comment lexbuf ; assembleur fft lexbuf}
 | sep* ident sep* ':' sep* (('\n'|'\r')?) {assembleur fft lexbuf}
 | sep* ('\n'|'\r') {assembleur fft lexbuf}
 | _ { raise (Assembleur_error "syntax_error")}
 | eof {}


and comment = parse 
 | "*)" {}
 | "(*" {comment lexbuf ; comment lexbuf}
 | eof {raise (Assembleur_error "unfinished comment\n")}
 | _ {comment lexbuf}
{
let options = ["-lines",Arg.Int (fun x -> nombre_lignes := x), "complète avec des lignes de 0 si nécessaire"]
let usage = "usage : assembleur file" 
let ifile = ref ""
let ofile = ref "" 
let set_file f s = f := s 
let main () = 
begin
 Arg.parse options (set_file ifile) usage ; 
 if !ifile="" then begin Format.eprintf "Aucun fichier à compiler\n@?"; exit 1 end; 

 if not (Filename.check_suffix !ifile ".asm") then begin
    Format.eprintf "Le fichier d'entrée doit avoir l'extension .asm\n@?";
    Arg.usage options usage;
    exit 1
  end;
  ofile := (Filename.chop_suffix !ifile ".asm") ^ ".rom" ;

 let f = open_in !ifile in
  let buf = Lexing.from_channel f in
  get_lab 0 buf ;
  close_in f ;
  let f2 = open_in !ifile in 
  let buf2 = Lexing.from_channel f2 in 
  let output = open_out !ofile in
  let out_fmt = Format.formatter_of_out_channel output in
  Format.fprintf out_fmt "%d 16\n" (!nombre_lignes) ;
  assembleur out_fmt buf2 ;
  if !nombre_lignes <0 then Format.eprintf "Warning : bad number of lines \n" ;
  while (!nombre_lignes > 0) do 
	Format.fprintf out_fmt "0000000000000000\n" ;
	decr nombre_lignes ;
  done ;
  Format.fprintf out_fmt "@." ;
  close_in f2 ;
  close_out output ;
end 

let () = main () 
}



