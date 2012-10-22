open Netlist_ast
open Def
(* plus de d�finition de clock qui est pass�e en argument *)
(*temps � attendre entre chaque execution, en ms, je sais pas trop ce qu'il faudra prendre, 
on pourra toujours demander lors du lancement du programme*)


let vnot = function 
	| VBit(b) -> VBit(not b)
	| VBitArray v -> VBitArray (Array.map (not) v)

(* Notez que telles que d�finies ici, les fonctions d'arit� multiple sont non-currifi�es
   i.e. op�rent sur des tuples, comme elles le faisaient avant la r��criture.
   Si on veut les currifier, c'est simple... *)
let vopbin op = function
  | (VBit b1, VBit b2) -> VBit (op b1 b2)
  | (VBitArray v1, VBitArray v2) -> let n = Array.length v1 in
                                    if Array.length v2 = n
                                    then VBitArray (Array.init n (fun i -> (op (v1.(i)) (v2.(i)))))
                                    else failwith "tableaux de taille incompatible"
  | _ -> failwith "impossible d'op�rer entre VBit et VBitArray"

let vor  = vopbin (||) and vxor  = vopbin (<>) (* xor = "diff�rent de" sur les bool�ens *)
and vand = vopbin (&&) and vnand = vopbin (fun x y -> not (x && y))

let vmux = function 
	| VBit(b),v1,v2 -> if b then v1 else v2 ;
	| _,_,_ -> failwith "la valeur donn�e en test est un tableau" 
	(*doit on d�finir le mux dans le cas o� la valeur de test est un tableau? 
	(en faisant une op�ration bit � bit sur les valeurs d'entr�e par exemple?) 
	*)

(*fonctions qui g�rent le cas particulier des registres*)			
let rec entree_reg tabvar = function 
	| (id, MEreg(k,k')) -> k' := tabvar.(k)
	| _ -> failwith "la liste attendue ne doit comporter que des registres"
	
let rec sortie_reg tabvar = function 
	| (id, MEreg(k,k')) -> tabvar.(id) <- !k'
	| _ -> failwith "la liste attendue ne doit comporter que des registres"

let extraction_VBit n = function
  | (VBit b) -> if n = 0 then VBit b
                         else failwith "acces un un bit inexistant"  
  | (VBitArray v) -> let nmax = Array.length v -1 in
                       if 0<=n & n<=nmax then VBit (v.(n))
                       else failwith "acces � un bit inexistant dans un tableau"

let concat_value = function 
  | (VBit b1, VBit b2) -> VBitArray [| b1;b2|]
  | (VBitArray v1, VBitArray v2) -> VBitArray (Array.append v1 v2)
  | (VBit b1, VBitArray v2) -> VBitArray (Array.append [|b1|] v2)
  | (VBitArray v1, VBit b2) -> VBitArray (Array.append v1 [|b2|])
  
let slice i j = function 
	| VBit(b) when i = 0 && j = 0 -> VBit(b)
	| VBitArray(v) when i <= j && j < Array.length v -> 
		let v = Array.init (j-i+1) (fun k -> v.(k+i)) in 
		VBitArray(v) 
	| _ -> failwith "acces a un bit inexistant" 


let apply_eq tabvar eq = 
	match snd (eq) with 
		| MEarg(k) ->   tabvar.(fst eq) <- tabvar.(k)
		| MEreg(_) ->  entree_reg tabvar eq ;
		| MEnot(k) ->  tabvar.(fst eq) <- vnot tabvar.(k)
		| MOr(k,k') ->  tabvar.(fst eq) <- vor (tabvar.(k),tabvar.(k'))
		| MAnd(k,k') ->  tabvar.(fst eq) <- vand (tabvar.(k),tabvar.(k'))
		| MNand(k,k') ->  tabvar.(fst eq) <- vnand (tabvar.(k),tabvar.(k'))	
		| MXor(k,k') ->  tabvar.(fst eq) <- vxor (tabvar.(k),tabvar.(k'))
		| MEmux(k,k1,k2) ->  tabvar.(fst eq) <- vmux (tabvar.(k),tabvar.(k1),tabvar.(k2))
		| MErom(_) -> failwith "Erom non impl�ment�"
		| MEram(_) -> failwith "Eram non impl�ment�"
		| MEslice(s1,s2,k) ->  tabvar.(fst eq) <- slice s1 s2 tabvar.(k)
		| MEselect(n,k) -> tabvar.(fst eq) <- extraction_VBit n (tabvar.(k)) 		
		| MEconcat(k1,k2) -> tabvar.(fst eq) <- concat_value (tabvar.(k1),tabvar.(k2))
		
		
		
(*fonctions pour retourner � l'�cran les valeurs des outputs*)
let print_value tabvar k = 
	let i = key_of_ident k in 
	begin
	match tabvar.(i) with 
		| VBit(b) -> if b then print_int 1 else print_int 0 ;
		| VBitArray(v) -> Array.iter (function b -> print_int (if b then 1 else 0)) v ; 
	end ;
	print_newline() 
	
let rec print_outputs tabvar = function 
	| [] -> () 
	| t::q -> print_string (t^ " : ") ; 
		print_value tabvar t ;
		print_outputs tabvar q 
	

(*fonction pour r�cup�rer les inputs de l'utilisateur en mode pas � pas, il faut lui passer en argument
la liste des inputs*)		
let rec get_inputs tabvar = function 
	| [] -> ()
	| t::q -> 
		print_string ("valeur de l'entree "^t^" ? \n") ;
		let s = read_line () in 
		let v = Array.init (String.length s) (function i -> s.[i] = '1' ) in
		tabvar.(key_of_ident t) <- VBitArray(v) ;
		get_inputs tabvar q
			

		
let rec execution_a_step mp m_option= 
	get_inputs (mp.mp_tabvar) (mp.mp_inputs) ;
	List.iter (sortie_reg (mp.mp_tabvar)) (mp.mp_special) ; 
(* mp_special ne contient que les registres jusqu'� maintenant *) 
	List.iter (apply_eq (mp.mp_tabvar)) (mp.mp_eqs) ;
	if m_option.overbose then print_outputs (mp.mp_tabvar) (mp.mp_outputs) 

	
let execution mp m_option = (* mp de type Mprogramme *) 
	
  if m_option.osteps  = -1 then 
    while(true) do
      execution_a_step mp m_option
    done
  else 
   for i = 1 to m_option.osteps do
     execution_a_step mp m_option
   done
   
(*fonction pour demander � l'utilisateur s'il souhaite continuer*)

let rec ask_continue () = 
	print_string "continuer ? (y/n) " ;
	match read_line() with 
		| "y" -> () ;
		| "n" -> raise (exit 1) ;
		| _ -> ask_continue () 
   
  (*fonction pour lancer la boucle du mode pas � pas*)
 let exec_debug mp m_option = 
	m_option.overbose <- true;
	while true do 
		execution_a_step mp m_option;
		ask_continue () 
	done 
		
		








	
