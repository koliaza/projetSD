open Netlist_ast
open Graph
open List


exception Combinational_cycle
 

let read_exp eq = 
 let read_arg a = match a with
  | Avar(id) -> [id] 
  | _ -> [] 
 in 
  let l = match snd eq with 
    | Earg(a) | Enot (a) -> read_arg a 
    | Ereg(a) -> [a]
    | Ebinop(a,b,c) -> (read_arg b)@(read_arg c)
    | Emux(a,b,c) ->(read_arg a)@(read_arg b)@(read_arg c)
    | Erom(a,b,c) -> read_arg c
    | Eram(a,b,c,d,e,f) -> (read_arg c)@(read_arg d)@(read_arg e)@(read_arg f)
    | Econcat(a,b) -> (read_arg a)@(read_arg b)
    | Eslice(a,b,c) -> read_arg c
    | Eselect(a,b) -> read_arg b
  in l

let rec enleve_element l lenleve =
 if l = [] then l
 else begin
 try 
  List.find (function t -> (t = hd l)) lenleve;
  enleve_element (tl l) lenleve
 with Not_found -> hd l ::(enleve_element (tl l) lenleve)
 end




let schedule p = 
  let gauche_liste =List.map (function eq -> fst eq) p.p_eqs in 
  let vertex_liste = p.p_inputs@gauche_liste in 
  let g = mk_graph() in 
  let liste_sortie_reg = ref ([]) in
      let gestion_edge eq =
           match eq with
              |(a,Ereg(_)) -> liste_sortie_reg:= a :: !liste_sortie_reg    
(* on ne met pas les liaisons des Registres car ils sont consid�r�s � la fois comme des inputs et des portes qui ont une entr�e.
   ils seront trait�s au d�but (manuellement) et � la fin (car mis � la fin de p_eqs) de l'�x�cution*)
              |_ -> List.iter (add_edge g (fst eq)) (read_exp eq) 
      in
  List.iter (add_node g) vertex_liste ;
  List.iter (fun eq -> gestion_edge eq) p.p_eqs;
      let rec renvoyer_eq l_a l_b =  
          match l_a with 
              | [] -> [] 
              | t::q -> (List.find (function eq -> fst eq = t) l_b)::(renvoyer_eq q l_b)
      in 
  {p_eqs = renvoyer_eq ( (enleve_element (List.rev (topological g)) (p.p_inputs@(!liste_sortie_reg))) @ (!liste_sortie_reg) ) p.p_eqs;
  p_inputs = p.p_inputs ;
  p_outputs = p.p_outputs ;
  p_vars = p.p_vars }
    
    
