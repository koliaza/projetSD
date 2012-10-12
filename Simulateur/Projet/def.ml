(* définition et construction des types que l'on va utiliser *)

open Netlist_ast (* chargement des anciens types *)

(* principales différentes:
les variables sont toutes dans un tableau (entrée, sorties, variables, constantes)


Tout commence par M car sinon il y aurait conflit de notation avec les anciens types 
*)

(* sorties , application entrées *)
type application =
  | int * MOr of int * int
  | int * MXor of int * int
  | int * MAnd of int * int
  | int * MNand of int * int
  | int * MEarg of int
  | int * MEreg of int
  | int * MENot of int
  | int * MEmux of int * int * int
  | int * MErom of int * int * int 
  | int * MEram of int * int * int * int * int * int
  | int * MEconcat of int * int
  | int * MEslice of int * int * int
  | int * MEselect of int * int

let convertion_eq_to_application eq =
  match eq with
    | (s,Earg a) -> (key_of_ident s, MEarg (key_of_arg a))
    | (s,Ereg a) -> (key_of_ident s, MEreg (key_of_ident a))
    | (s,ENot a) -> (key_of_ident s, MEreg (key_of_arg a))
    | (s,Ebinop (binop, a, b)) -> begin
                    match binop with
		      | Or -> (key_of_ident s, MOr (key_of_arg a,key_of_arg b))
		      | Xor ->(key_of_ident s, MXor(key_of_arg a,key_of_arg b))
		      | And ->(key_of_ident s, MAnd(key_of_arg a,key_of_arg b))
		      | Nand->(key_of_ident s,MNand(key_of_arg a,key_of_arg b))
    | (s,Emux (a, b, c)) -> (key_of_ident s,MEmux (key_of_arg a, key_of_arg b,
						  key_of_arg c))
    (* attention : bien faire attention au fait que certaines fonctions
       ont des paramètres entiers qui ne correspondront pas à une variable
       voir la définition de Erom, Eram, Eslice, Eselect *)
    | (s,Erom (a, b, c)) -> (key_of_ident s, MErom (a, b, key_of_arg c))
    | (s,Eram (a, b, c, d, e, f)) -> (key_of_ident s, MEram (a, b, 
                                     key_of_arg c, key_of_arg d,
                                     key_of_arg e, key_of_arg f))
    | (s,Econcat (a, b)) -> (key_of_ident s, MEconcat (key_of_arg a,
                                                      key_of_arg b))
    | (s,Eslice (a, b, c)) -> (key_of_ident s, MEslice (a, b, key_of_arg c))
    | (s,Eselect (a, b)) -> (key_of_ident s, MEselect (a, key_of_arg b))


type program =
    { p_eqs : equation list;
      p_inputs : ident list;
      p_outputs : ident list;
      p_vars : ty Env.t; }
