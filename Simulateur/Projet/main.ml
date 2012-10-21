(* pas d'open necessaires pour le main *)
(* definition des options *)
let oprint = ref false
let oschedule = ref false
let odebug = ref false
let overbose = ref false
let osteps = ref -1
(*fin des options *)

let option_list = 
    ["-scheduleonly", Arg.Set oschedule, "Stops after printing the result of scheduling";
	"-print", Arg.Clear oprint, "Does not print the result of scheduling";
	"-debug", Arg.set odebug, "prints step by step and waits for user control"; 
     "-n", Arg.Set_int osteps, "Number of steps to simulate"
	 "-verbose", Arg.Set overbose, "print all the outputs"]
   
let main_exec filename= 
 try
    let netp = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let out = open_out out_name in
    let close_all () =
      close_out out
    in
    begin try
        let p = Scheduler.schedule netp in
		if !oprint then
			Netlist_printer.print_program out p; (* Netlist_printer est � modifier pour indiquer comment est trait� le cas des registres *)
		else () 
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
            close_all (); exit 2
    in
    close_all ();
    if not !oschedule then (
		let mp = conversion_programme p in
		  if odebug then
		     Execution.exec_debug mp   (* koliaza: Marc veut que tu lui passes une variable de plus pour afficher les sorties : regarde son code aux lignes o� il y a !Main. *)
		  else
		     Execution.execution !osteps mp
		)
  with
    | Netlist.Parse_error s -> Format.eprintf "A Netlist error accurred: %s@." s; exit 2

let main () = 	
	Arg.parse 
		option_list 
		main_exec 
		""
;;

main()
