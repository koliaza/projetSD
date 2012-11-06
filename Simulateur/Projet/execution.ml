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
                                    then VBitArray (Array.init n (fun i -> (op (v1.(i)) (v2.(i)))))
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

	
(*fonction qui convertit une adresse donnée en VBitArray en indice dans le tableau tabram ou tabrom correspondant
le bit de poinds faible est celui à l'indice 0 du tableau*)	
let adr_to_int = function 
	| VBit(b) -> if b then 1 else 0
	| VBitArray(v) -> let k = ref 0 in 
		for i = Array.length v -1 downto 0 do 
			k := 2* !k + (if v.(i) then 1 else 0)
		done ;
		!k 
		
(*fonctions qui gèrent le cas particulier des memoires*)	
let entree_mem p = function 
	| (_, MEreg(k,k')) -> k' := p.mp_tabvar.(k) ;
	| (_, MEram(_,_,_,we,wa,data)) -> let VBit(b) = p.mp_tabvar.(we) in 
		if b then 
		p.mp_tabram.(adr_to_int p.mp_tabvar.(wa)) <- p.mp_tabvar.(data)
	| (_,MErom(_)) -> ()
	| _ -> assert false
		
let sortie_reg tabvar = try function 
	| (id, MEreg(k,k')) -> tabvar.(id) <- !k'
	| (_,MErom(_)) | (_,MEram(_)) -> () ;
	| _ -> assert false
with _ -> failwith "" 

let extraction_VBit n = function
  | (VBit b) -> if n = 0 then VBit b
                         else failwith "acces un un bit inexistant"  
  | (VBitArray v) -> let nmax = Array.length v -1 in
                       if 0<=n & n<=nmax then VBit (v.(n))
                       else failwith "acces à un bit inexistant dans un tableau"

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

	
let apply_eq p eq =
	let tabvar = p.mp_tabvar in
	match snd (eq) with 
		| MEarg(k) ->   tabvar.(fst eq) <- tabvar.(k)
		| MEreg(k,k') -> ()
		| MEnot(k) ->  tabvar.(fst eq) <- vnot tabvar.(k)
		| MOr(k,k') ->  tabvar.(fst eq) <- vor (tabvar.(k),tabvar.(k'))
		| MAnd(k,k') ->  tabvar.(fst eq) <- vand (tabvar.(k),tabvar.(k'))
		| MNand(k,k') ->  tabvar.(fst eq) <- vnand (tabvar.(k),tabvar.(k'))	
		| MXor(k,k') ->  tabvar.(fst eq) <- vxor (tabvar.(k),tabvar.(k'))
		| MEmux(k,k1,k2) ->  tabvar.(fst eq) <- vmux (tabvar.(k),tabvar.(k1),tabvar.(k2))
		| MErom(_,_,k) -> tabvar.(fst eq) <- p.mp_tabrom.(adr_to_int tabvar.(k))
		| MEram(_,_,k,_,_,_) ->tabvar.(fst eq) <- p.mp_tabram.(adr_to_int tabvar.(k)) 
		| MEslice(s1,s2,k) ->  tabvar.(fst eq) <- slice s1 s2 tabvar.(k)
		| MEselect(n,k) -> tabvar.(fst eq) <- extraction_VBit n (tabvar.(k)) 		
		| MEconcat(k1,k2) -> tabvar.(fst eq) <- concat_value (tabvar.(k1),tabvar.(k2))	
		
		
(*fonctions pour retourner à l'écran les valeurs des outputs*)
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
	

let type_to_string = function 
	| TBit -> "(1 bit)"
	| TBitArray(n) -> "(" ^ (string_of_int n) ^ " bit)"
	
(*fonction pour récupérer les inputs de l'utilisateur en mode pas à pas, il faut lui passer en argument
la liste des inputs, et l'environnement qui donne le type de chaque variable*)		
let rec get_inputs tabvar env = function 
	| [] -> ()
	| var::q -> 
		let t = Env.find var env in
		print_string ("valeur de l'entree "^var^ (type_to_string t) ^" ? \n") ;
		let s = read_line () in 
		begin
		match t with 
		| TBit when String.length s = 1 ->  tabvar.(key_of_ident var) <- VBit(s.[0] = '1')
		| TBitArray(n) when String.length s = n -> 
			let v = Array.init (String.length s) (function i -> s.[i] = '1' ) in
			tabvar.(key_of_ident var) <- VBitArray(v) ;
		| _ -> failwith "le nombre de bit passe en argument ne correspond pas a celui attendu"
		end ;
		get_inputs tabvar env q 
;;
			
			

		
let rec execution_a_step mp m_option= 
	get_inputs (mp.mp_tabvar) mp.mp_vars (mp.mp_inputs) ;
	List.iter (sortie_reg (mp.mp_tabvar)) (mp.mp_special) ; 
(* mp_special ne contient que les registres jusqu'à maintenant *) 
	List.iter (apply_eq mp) (mp.mp_eqs) ;
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
		
		








	
