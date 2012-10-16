(*open???*)

(* pour la lisibilite on le laisse actuellement a l'exterieur.
Mais on le mettra dans execution pour qu'il puisse avoir acces
a t *)

(*on d�finit les foncitons sur les valeurs*)

let freq = ref 1.
(*temps � attendre entre chaque execution, en ms, je sais pas trop ce qu'il faudra prendre, 
on pourra toujours demander lors du lancement du programme*)


let vnot = function 
	| VBit(b) -> Vbit(not b)
	| VBitArray(v) -> Array.init (Array.length v) (fun i -> not v.(i))

let vor = function 
	| VBit(b1), VBit(b2) -> Vbit(b1 || b2)
	| VBitArray(v1), VBitArray(v2) -> if Array.length v1 = Array.length v2 then 
		VBitArray(Array.init (fun i -> v1.(i) || v2.(i))) 
		else failwith "tableaux de taille incompatible"
	| _,_ -> failwith "impossible d'op�rer entre VBit et VBitArray"

let vand = function 
	| VBit(b1), VBit(b2) -> Vbit(b1 && b2)
	| VBitArray(v1), VBitArray(v2) -> if Array.length v1 = Array.length v2 then 
		VBitArray(Array.init (fun i -> v1.(i) && v2.(i))) 
		else failwith "tableaux de taille incompatible"
	| _,_ -> failwith "impossible d'op�rer entre VBit et VBitArray"

let vxor = function 
	| VBit(b1), VBit(b2) -> Vbit((b1 && (not b2))||((not b1)&& b2))
	| VBitArray(v1), VBitArray(v2) -> if Array.length v1 = Array.length v2 then 
		VBitArray (
		Array.init Array.length v1 
		(fun i -> (v1.(i) && (not v2.(i)))||((not v1.(i))&& v2.(i)))
		)
		else failwith "tableaux de taille incompatible"
	| _,_ -> failwith "impossible d'op�rer entre VBit et VBitArray"

		
let vand = function 
	| VBit(b1), VBit(b2) -> Vbit(not (b1 && b2))
	| VBitArray(v1), VBitArray(v2) -> if Array.length v1 = Array.length v2 then 
		VBitArray(Array.init (fun i -> not (v1.(i) && v2.(i)))) 
		else failwith "tableaux de taille incompatible"
	| _,_ -> failwith "impossible d'op�rer entre VBit et VBitArray"
	
let vmux = function 
	| VBit(b),v1,v2 -> if b then v1 else v2 ;
	| _,_,_ -> failwith "la valeur donn�e en test est un tableau" 
	(*doit on d�finir le mux dans le cas o� la valeur de test est un tableau? 
	(en faisant une op�ration bit � bit sur les valeurs d'entr�e par exemple?) 
	*)


(*Remarque, les registres doivent �tre trait�s A PART*)	
let apply_eq eq = 
	match snd (eq) with 
		| MEarg(k) ->   init_tableau.(fst eq) <- init_tableau.(k)
		| MEreg(_) ->  () ;
		| MENot(k) ->  init_tableau.(fst eq) <- vnot init_tableau.(k)
		| MOr(k,k') ->  init_tableau.(fst eq) <- vor (init_tableau.(k),init_tableau.(k'))
		| MAnd(k,k') ->  init_tableau.(fst eq) <- vand (init_tableau.(k),init_tableau.(k'))
		| MNand(k,k') ->  init_tableau.(fst eq) <- vnand (init_tableau.(k),init_tableau.(k'))	
		| MXor(k,k') ->  init_tableau.(fst eq) <- vxor (init_tableau.(k),init_tableau.(k'))
		| MMux(k,k1,k2) ->  init_tableau.(fst eq) <- vmux (init_tableau.(k),init_tableau.(k1),init_tableau(k2))
		| MErom(_) -> failwith "non impl�ment�"
		| MEram(_) -> failwith "non impl�ment�"
		| MEslice(_) -> failwith "non impl�ment�"
		| MEselect(_) -> failwith "non impl�ment�"		
		| MEconcat(_) -> failwith "non impl�ment�"
		
		
		
(*fonction pour r�cup�rer les inputs de l'utilisateur en mode pas � pas, il faut lui passer en argument
la liste des inputs*)		
let rec get_inputs = function 
	| [] -> ()
	| t::q -> 
		print_string ("valeur de l'entr�e "^t^" ?) \n") ;
		let n = ref int_of_string("0b" ^ (read_line ())) in 
		let v = if n <=1 then VBit (!n = 1) 
			else 
			let l = ref [] in 
			while !n <> 0 do 
			l := (n mod 2)::(!l) ;
			n := n/2 ;
			done ;
		VBitArray (Array.of_list (List.rev !l))
		in init_tableau.(key_of_ident t) <- v ;
		get_inputs q
			

(*fonctions qui g�rent le cas particulier des registres*)			
let rec entree_reg = function 
	| [] -> () 
	| (id, MEreg(k,k')) -> k' := init_tableau.(k)
	| _ -> failwith "la liste attendue ne doit comporter que des registres"
	
let rec sortie_reg = function 
	| [] -> () 
	| (id, MEreg(k,k')) -> init_tableau.(id) <- !k'
	| _ -> failwith "la liste attendue ne doit comporter que des registres"
		
let rec execution_a_step p_eqs = 
	get_inputs () ;
	List.iter sortie_reg reg_liste ;(*il faut d�finir une liste ne contenant que les �quations de registre *)
	List.iter entree_reg reg_liste ;
	List.iter apply_eq p_eqs ;

	
(*fonctions pour retourner � l'�cran les valeurs des outputs*)
let print_value k = 
	let i = key_of_indent k in 
	begin
	match init_tableau.(i) with 
		| VBit(b) -> if b then print_int 1 else print_int 0 ;
		| VBitArray(v) -> Array.iter (function b -> print_int (if b then 1 else 0)) v ; 
	end ;
	print_newline() ;
	
let rec print_outputs = function 
	| [] -> () 
	| t::q -> print_string (t^ " : ") ; 
		print_value t ;
		print_outputs q ;
	

let execution n p_eqs = 
  if n = -1 then 
    while(true) do
      execution_a_step p_eqs
    done;
  else 
   for i = 1 to n do
     execution_a_step p_eqs
   done;
   
(*fonction pour demander � l'utilisateur s'il souhaite continuer*)

let ask_continue () = 
	print_string "continuer ? (y/n) " ;
	match read_line() with 
		| "y" -> () ;
		| "n" -> raise (exit 1) ;
		| _ -> ask_continue () ;
   
  (*fonction pour lancer la boucle du mode pas � pas*)
 let exec_debug p_eqs = 
	while true do 
		execution_a_step p_eqs ;
		print_outputs (out_liste) ;(*il faut voir comment on acc�de � out_liste*)
		ask_continue () ;
	done ;;
		
		








	
