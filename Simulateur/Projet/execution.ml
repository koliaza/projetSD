(*open???*)

(* pour la lisibilite on le laisse actuellement a l'exterieur.
Mais on le mettra dans execution pour qu'il puisse avoir acces
a t *)

(*on définit les foncitons sur les valeurs*)

let vnot = function 
	| VBit(b) -> Vbit(not b)
	| VBitArray(v) -> Array.init (Array.length v) (fun i -> not v.(i))

let vor = function 
	| VBit(b1), VBit(b2) -> Vbit(b1 || b2)
	| VBitArray(v1), VBitArray(v2) -> if Array.length v1 = Array.length v2 then 
		VBitArray(Array.init (fun i -> v1.(i) || v2.(i))) 
		else failwith "tableaux de taille incompatible"
let vand = function 
	| VBit(b1), VBit(b2) -> Vbit(b1 && b2)
	| VBitArray(v1), VBitArray(v2) -> if Array.length v1 = Array.length v2 then 
		VBitArray(Array.init (fun i -> v1.(i) && v2.(i))) 
		else failwith "tableaux de taille incompatible"

let vxor = function 
	| VBit(b1), VBit(b2) -> Vbit((b1 && (not b2))||((not b1)&& b2))
	| VBitArray(v1), VBitArray(v2) -> if Array.length v1 = Array.length v2 then 
		VBitArray (
		Array.init Array.length v1 
		(fun i -> (v1.(i) && (not v2.(i)))||((not v1.(i))&& v2.(i)))
		)
		else failwith "tableaux de taille incompatible"
		
let vand = function 
	| VBit(b1), VBit(b2) -> Vbit(not (b1 && b2))
	| VBitArray(v1), VBitArray(v2) -> if Array.length v1 = Array.length v2 then 
		VBitArray(Array.init (fun i -> not (v1.(i) && v2.(i)))) 
		else failwith "tableaux de taille incompatible"
		
(*je sais pas si c'est une bonne idée, en tout cas au pire, je peux réutiliser le match, en virant les fun () ->*)		
let fun_of_eqs eq = 
	match snd (eq) with 
		| MEarg(k) -> fun () ->  failwith "à quoi correspond arg?" 
		| MEreg(k) -> fun () -> failwith "non implémenté" ;
		| MENot(k) -> fun () -> init_tableau.(fst eq) <- not init_tableau.(k)
		| MOr(k,k') -> fun () -> 
			init_tableau.(fst eq) <- vor (init_tableau.(k),init_tableau.(k'))
		| MAnd(k,k') -> fun () -> 
			init_tableau.(fst eq) <- vand (init_tableau.(k),init_tableau.(k'))
		| MNand(k,k') -> fun () -> 
			init_tableau.(fst eq) <- vnand (init_tableau.(k),init_tableau.(k'))	
		| MXor(k,k') -> fun () -> 
			init_tableau.(fst eq) <- vxor (init_tableau.(k),init_tableau.(k'))
		| MMux(k,k') -> fun () -> 
			(*suite à faire*)

let tableexe = List.map (function eq -> fun_of_eq eq) p_eqs

let execution_a_step () =
	let rec aux = function 
		| [] -> () ;
		| t::q -> t () ; aux q 
	in aux ;;




let execution t n p_eqs = 
  if n = -1 then 
    while(true) do
      execution_a_step p_eqs
    done;
  else 
   for i = 1 to n do
     execution_a_step p_eqs
   done;







 
