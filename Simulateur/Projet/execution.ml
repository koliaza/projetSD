(*open ????*)


(* pour la lisibilite on le laisse actuellement a l'exterieur.
Mais on le mettra dans execution pour qu'il puisse avoir acces
a t *)

let execution_a_step p_eqs









let execution t n p_eqs = 
  if n = -1 then 
    while(true) do
      execution_a_step p_eqs
    done;
  else 
   for i = 1 to n do
     execution_a_step p_eqs
   done;







 
