(* pas d'open necessaires pour le main à part Def *)
(* definition des options *)
open Def
				
let default = {oprint = false; oschedule = false; odebug = false;
			   overbose = false; osteps = -1; oclock = 1.}
(* on modifie le default au fur et a mesure *)
(*fin des options *)

let option_list = 
    ["-scheduleonly", Arg.Unit (fun () -> default.oschedule <- true),
                     "Stops after printing the result of scheduling" ;
	"-noprint", Arg.Unit (fun () -> default.oprint <- false),
                "Does not print the result of scheduling";
	"-debug", Arg.Unit (fun () -> default.odebug <- true),
              "prints step by step and waits for user control"; 
     "-n", Arg.Int (fun x -> default.osteps <- x),
           "Number of steps to simulate";
	 "-clock", Arg.Float (fun x -> default.oclock <- x),
               "clock speed to simulate";
	 "-verbose", Arg.Unit (fun () -> default.overbose <- true),
                 "print all the outputs"]
   
let main_exec filename= 
    let netp = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let out = open_out out_name in
    let close_all () =
      close_out out
    in
    begin try
        let p = Scheduler.schedule netp in
		if default.oprint then
			Netlist_printer.print_program out p; (* Netlist_printer est à modifier pour indiquer comment est traité le cas des registres *)
		if not default.oschedule then (
			let mp = conversion_programme p in
				 if default.odebug then
				  Execution.exec_debug mp default  (* koliaza: Marc veut que tu lui passes une variable de plus pour afficher les sorties : regarde son code aux lignes où il y a !Main. *)
				else
				  Execution.execution  mp default
			) 
	  with        
		| Scheduler.Combinational_cycle ->
		    Format.eprintf "The netlist has a combinatory cycle.@.";
		    close_all (); exit 2
		| Netlist.Parse_error s -> Format.eprintf "A Netlist error accurred: %s@." s; exit 2
	end;
    close_all ()

let main () = Arg.parse option_list main_exec ""

let _ = main ()
