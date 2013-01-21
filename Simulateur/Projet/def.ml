(* definition et construction des types que l'on va utiliser *)

open Netlist_ast (* chargement des anciens types *)
open Hashtbl
(* type pour la gestion des options dans le main *) 
type  moption = {mutable oprint : bool; mutable oschedule : bool;
				mutable odebug : bool; mutable overbose : bool;
				mutable optionclock : bool; mutable compare :bool;
				mutable osteps : int; mutable clockfreq : float;
				mutable ram_file :string; mutable rom_file : string;
				mutable ramlist : int list}
(* principales differences:
les variables sont toutes dans un tableau (entree, sorties, variables, 
constantes)


Tout commence par m car sinon il y aurait conflit de notation avec les 
anciens types 
*)

(* sorties , application entrees *)


type mconstructeur =
  | MOr of int * int
  | MXor of int * int
  | MAnd of int * int
  | MNand of int * int
  | MEarg of int
  | MEreg of int * (value ref)   (* reference sur la derniere valeur entree *)
  | MEnot of int
  | MEmux of int * int * int
  | MErom of int(*adr size*) * int(*word size*) * int (*read adr*)
  | MEram of int(*adr size*) * int (*word size*)* int(*read adr*) 
      * int (*write enable*)* int (*write adr*)* int (*data*)
  | MEconcat of int * int
  | MEslice of int * int * int
  | MEselect of int * int

type application = int * mconstructeur

type mprogram =
    { mp_eqs : application list;
      mp_inputs : ident list;
      mp_outputs : ident list;
      mp_vars : ty Env.t; 
      mp_tabvar : value array ;
      mp_special : application list;
      mutable mp_tabram : value array ;
      mutable mp_tabrom : value array}

(* id est du type string = ident.
on place dans un table de hachage la cle donnee a id si elle n'existe pas deja.
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
         Hashtbl.find constvsint c
       with Not_found ->
         Hashtbl.add constvsint c (!lastkeygiven +1);
         lastkeygiven := !lastkeygiven +1;
         list_const := (!lastkeygiven,c) :: !list_const; 
         !lastkeygiven


(* on ajoute les registres dans !mp_special par effet de bord *)
let conversion_eq_to_application mp_special eq = 
  match eq with
    | (s,Earg a) -> (key_of_ident s, MEarg (key_of_arg a))
    | (s,Ereg a) -> let resultat = (key_of_ident s, MEreg (key_of_ident a, ref(VBit false))) in
                    mp_special := resultat :: (!mp_special);
                    resultat
    | (s,Enot a) -> (key_of_ident s, MEnot (key_of_arg a))
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
    | (s,Erom (a, b, c)) -> let resultat = (key_of_ident s, MErom (a, b, key_of_arg c))
                            in
                              mp_special := resultat :: (!mp_special);
                              resultat
    | (s,Eram (a, b, c, d, e, f)) -> let resultat = (key_of_ident s, MEram (a, b, 
                                     key_of_arg c, key_of_arg d,
                                     key_of_arg e, key_of_arg f))
                                     in
                                       mp_special := resultat :: (!mp_special);
                                       resultat
    | (s,Econcat (a, b)) -> (key_of_ident s, MEconcat (key_of_arg a,
                                                      key_of_arg b))
    | (s,Eslice (a, b, c)) -> (key_of_ident s, MEslice (a, b, key_of_arg c))
    | (s,Eselect (a, b)) -> (key_of_ident s, MEselect (a, key_of_arg b))


(* hypothese : toutes les cles necessaires ont ete donnees*)
let init_tableau () =
  let t = Array.make (!lastkeygiven +1) (VBit false) in
  let lconst_restant = ref(!list_const) in
    while !lconst_restant <> [] do
      let (key,la_const) = List.hd (!lconst_restant) in
        t.(key) <- la_const;
        lconst_restant := List.tl( !lconst_restant);
    done;
  t

(*les deux fonctions pour initialiser les tableaux de ram et de roms ne 
prennent pas en compte le cas où il y aurait plusieurs ram ou rom dans le 
circuit*)

let rec init_rom = function 
  | [] -> [||]
  | (_,MErom(ads,ws,_))::_ ->
      Array.make (1 lsl ads) (VBitArray (Array.make ws false ))
        (*il faudrait rajouter ici une fonction qui initialise correctement les 
          valeurs dans la rom, en allant les chercher dans un fichier par ex *)
  | _::q -> init_rom q

let rec init_ram = function
  | [] -> [||]
  | (_,MEram(ads,ws,_,_,_,_))::_ ->  
      Array.make (1 lsl ads) (VBitArray (Array.make ws false ))
  | _::q -> init_ram q


let conversion_programme p =
  let mlist_special = ref([]) in
  let () = List.iter (fun inp -> let _ = key_of_ident inp in () ) p.p_inputs in
  let new_eqs = List.map (fun eq -> conversion_eq_to_application mlist_special eq) p.p_eqs in
  {  mp_eqs = new_eqs ;
     mp_inputs = p.p_inputs;
     mp_outputs = p.p_outputs;
     mp_vars  = p.p_vars; 
     mp_tabvar = init_tableau();
     mp_special = !mlist_special;
     mp_tabrom = init_rom new_eqs;
     mp_tabram = init_ram new_eqs}
      
