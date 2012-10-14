(* définition et construction des types que l'on va utiliser *)

open Netlist_ast (* chargement des anciens types *)
open Hashtbl
(* principales différentes:
les variables sont toutes dans un tableau (entrée, sorties, variables, constantes)


Tout commence par M car sinon il y aurait conflit de notation avec les anciens types 
*)

(* sorties , application entrees *)
type application =
  | int * MOr of int * int
  | int * MXor of int * int
  | int * MAnd of int * int
  | int * MNand of int * int
  | int * MEarg of int
  | int * MEreg of int * ref(value)   (* reference sur la derniere valeur entree *)
  | int * MENot of int
  | int * MEmux of int * int * int
  | int * MErom of int * int * int 
  | int * MEram of int * int * int * int * int * int
  | int * MEconcat of int * int
  | int * MEslice of int * int * int
  | int * MEselect of int * int

(* id est du type string = ident.
on place dans un table de hachage la clée donnée à id si elle n'existe pas deja.
On renvoie la cle associee.
Une fois tout les cles donnees, il faut ensuite creer le tableau contenant les
 variables 
la taille du tableau sera probablement nb variables + nb constantes*)

let (identvsint : (ident, int) Hashtbl.t) = Hashtbl.create 53 
(* la taille importe peu, on ne fait pas un truc optimise*)
let (constvsint : (value, int) Hashtbl.t) = Hashtbl.create 53

let list_const = ref []

let lastkeygiven = ref(-1) 
(* sert pour donner une nouvelle cle dans le tableau final *)



let key_of_ident id =
  try 
    Hashtbl.find identvsint id  
(* renvoie la cle si elle a deja ete donnee pour ce ident *)
  with Not_found ->
    Hashtbl.add identvsint id (!lastkeygiven +1);
    lastkeygiven := !lastkeygiven +1;
    !lastkeygiven
(* renvoie la nouvelle cle *)

let key_of_arg a =
  match a with
    |Avar id -> key_of_ident id
    |Aconst c ->
       try 
         Hashtbl.find constvsint id
       with Not_found ->
         Hashtbl.add constvsint id (!lastkeygiven +1);
         lastkeygiven := !lastkeygiven +1;
         list_const := (!lastkeygiven,c) :: !list_const; 
         !lastkeygiven



let convertion_eq_to_application eq =
  match eq with
    | (s,Earg a) -> (key_of_ident s, MEarg (key_of_arg a))
    | (s,Ereg a) -> (key_of_ident s, MEreg (key_of_ident a, ref(Vbit false)))
    | (s,ENot a) -> (key_of_ident s, MEreg (key_of_arg a))
    | (s,Ebinop (binop, a, b)) -> begin
                    match binop with
		      | Or -> (key_of_ident s, MOr (key_of_arg a,key_of_arg b))
		      | Xor ->(key_of_ident s, MXor(key_of_arg a,key_of_arg b))
		      | And ->(key_of_ident s, MAnd(key_of_arg a,key_of_arg b))
		      | Nand->(key_of_ident s,MNand(key_of_arg a,key_of_arg b))
                     end
    | (s,Emux (a, b, c)) -> (key_of_ident s,MEmux (key_of_arg a, key_of_arg b,
						  key_of_arg c))
    (* attention : bien faire attention au fait que certaines fonctions
       ont des parametres entiers qui ne correspondront pas a une variable
       voir la definition de Erom, Eram, Eslice, Eselect *)
    | (s,Erom (a, b, c)) -> (key_of_ident s, MErom (a, b, key_of_arg c))
    | (s,Eram (a, b, c, d, e, f)) -> (key_of_ident s, MEram (a, b, 
                                     key_of_arg c, key_of_arg d,
                                     key_of_arg e, key_of_arg f))
    | (s,Econcat (a, b)) -> (key_of_ident s, MEconcat (key_of_arg a,
                                                      key_of_arg b))
    | (s,Eslice (a, b, c)) -> (key_of_ident s, MEslice (a, b, key_of_arg c))
    | (s,Eselect (a, b)) -> (key_of_ident s, MEselect (a, key_of_arg b))


(* hypothese : toutes les cles necessaires ont ete donnees*)
let init_tableau =
  let t = Array.make (!lastkeygiven +1) (Vbit false) in
  let lconst_restant = ref lconst in
    while !lconst_restant <> [] do
      let (key,la_const) = hd (!lconst_restant) in
        t.(key) <- la_const;
        lconst_restant := tl( !lconst_restant);
    done;
  t


    
