open Netlist_ast
open Def
(* plus de définition de clock qui est passée en argument *)
(*temps à attendre entre chaque execution, en ms, je sais pas trop ce qu'il faudra prendre, 
on pourra toujours demander lors du lancement du programme*)


let vnot = function 
	| VBit(b) -> VBit(not b)
	| VBitArray v -> VBitArray (Array.map (not) v)

(* Notez que telles que définies ici, les fonctions d'arité multiple sont non-currifiées
   i.e. opérent sur des tuples, comme elles le faisaient avant la réécriture.
   Si on veut les currifier, c'est simple... *)
let vopbin op = function
  | (VBit b1, VBit b2) -> VBit (op b1 b2)
  | (VBitArray v1, VBitArray v2) -> let n = Array.length v1 in
                                    if Array.length v2 = n
                                    then VBitArray (Array.init n (fun i -> op v1.(i) v2.(i)))
                                    else failwith "tableaux de taille incompatible"
  | _ -> failwith "impossible d'opérer entre VBit et VBitArray"

let vor  = vopbin (||) and vxor  = vopbin (<>) (* xor = "différent de" sur les booléens *)
and vand = vopbin (&&) and vnand = vopbin (fun x y -> not (x && y))

let vmux = function 
	| VBit(b),v1,v2 -> if b then v1 else v2 ;
	| _,_,_ -> failwith "la valeur donnée en test est un tableau" 
	(*doit on définir le mux dans le cas où la valeur de test est un tableau? 
	(en faisant une opération bit à bit sur les valeurs d'entrée par exemple?) 
	*)

(*fonctions qui gèrent le cas particulier des registres*)			
let rec entree_reg = function 
	| (id, MEreg(k,k')) -> k' := init_tableau.(k)
	| _ -> failwith "la liste attendue ne doit comporter que des registres"
	
let rec sortie_reg = function 
	| (id, MEreg(k,k')) -> init_tableau.(id) <- !k'
	| _ -> failwith "la liste attendue ne doit comporter que des registres"

let apply_eq eq = 
	match snd (eq) with 
		| MEarg(k) ->   init_tableau.(fst eq) <- init_tableau.(k)
		| MEreg(_) ->  entree_reg eq ;
		| MEnot(k) ->  init_tableau.(fst eq) <- vnot init_tableau.(k)
		| MOr(k,k') ->  init_tableau.(fst eq) <- vor (init_tableau.(k),init_tableau.(k'))
		| MAnd(k,k') ->  init_tableau.(fst eq) <- vand (init_tableau.(k),init_tableau.(k'))
		| MNand(k,k') ->  init_tableau.(fst eq) <- vnand (init_tableau.(k),init_tableau.(k'))	
		| MXor(k,k') ->  init_tableau.(fst eq) <- vxor (init_tableau.(k),init_tableau.(k'))
		| MEmux(k,k1,k2) ->  init_tableau.(fst eq) <- vmux (init_tableau.(k),init_tableau.(k1),init_tableau.(k2))
		| MErom(_) -> failwith "non implémenté"
		| MEram(_) -> failwith "non implémenté"
		| MEslice(_) -> failwith "non implémenté"
		| MEselect(_) -> failwith "non implémenté"		
		| MEconcat(_) -> failwith "non implémenté"
		
		
		
(*fonctions pour retourner à l'écran les valeurs des outputs*)
let print_value k = 
	let i = key_of_ident k in 
	begin
	match init_tableau.(i) with 
		| VBit(b) -> if b then print_int 1 else print_int 0 ;
		| VBitArray(v) -> Array.iter (function b -> print_int (if b then 1 else 0)) v ; 
	end ;
	print_newline() 
	
let rec print_outputs = function 
	| [] -> () 
	| t::q -> print_string (t^ " : ") ; 
		print_value t ;
		print_outputs q 
	

(*fonction pour récupérer les inputs de l'utilisateur en mode pas à pas, il faut lui passer en argument
la liste des inputs*)		
let rec get_inputs = function 
	| [] -> ()
	| t::q -> 
		print_string ("valeur de l'entrée "^t^" ?) \n") ;
		let n = ref (int_of_string ("0b" ^ (read_line ()))) in 
		let v = if !n <=1 then VBit (!n = 1) 
			else 
			let l = ref [] in 
			while !n <> 0 do 
			l := ((!n mod 2) = 1)::(!l) ;
			n := !n / 2 ;
			done ;
		VBitArray (Array.of_list (List.rev !l))
		in init_tableau.(key_of_ident t) <- v ;
		get_inputs q
			

		
let rec execution_a_step mp m_option= 
	get_inputs (mp.mp_inputs) ;
	List.iter sortie_reg (mp.mp_special) ; 
(* Mp_special ne contient que les registres jusqu'à maintenant *) 
	List.iter apply_eq (mp.mp_eqs) ;
	if m_option.overbose then print_outputs (mp.mp_outputs) 

	
let execution mp m_option = (* mp de type Mprogramme *) 
	
  if m_option.osteps  = -1 then 
    while(true) do
      execution_a_step mp m_option
    done
  else 
   for i = 1 to m_option.osteps do
     execution_a_step mp m_option
   done
   
(*fonction pour demander à l'utilisateur s'il souhaite continuer*)

let rec ask_continue () = 
	print_string "continuer ? (y/n) " ;
	match read_line() with 
		| "y" -> () ;
		| "n" -> raise (exit 1) ;
		| _ -> ask_continue () 
   
  (*fonction pour lancer la boucle du mode pas à pas*)
 let exec_debug mp m_option = 
	m_option.overbose <- true;
	while true do 
		execution_a_step mp m_option;
		ask_continue () 
	done 
		
		








	
