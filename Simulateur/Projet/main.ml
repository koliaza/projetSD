(* pas d'open necessaires pour le main à part Def *)
(* definition des options *)
open Def
				
let options = {oprint = true; oschedule = false; odebug = false; 
			   overbose = false; osteps = -1; oclock = 1.; 
			   ramlist = [] ;
			   ram_file=""; rom_file=""}
(* on modifie les options au fur et a mesure *)
(*fin des options *)

let option_list = 
    ["-scheduleonly", Arg.Unit (fun () -> options.oschedule <- true),
                     "Stops after printing the result of scheduling" ;
	 "-noprint", Arg.Unit (fun () -> options.oprint <- false),
                "Does not print the result of scheduling";
	 "-debug", Arg.Unit (fun () -> options.odebug <- true),
              "prints step by step and waits for user control"; 
	 "-n", Arg.Int (fun x -> options.osteps <- x),
           "Number of steps to simulate";
	 "-clock", Arg.Float (fun x -> options.oclock <- x),
               "clock speed to simulate";
	 "-verbose", Arg.Unit (fun () -> options.overbose <- true),
                 "print all the outputs";
	 "-ramfile", Arg.String (fun s -> options.ram_file <- s),
				 "address of the ram file";
	 "-romfile", Arg.String (fun s -> options.rom_file <- s),
				 "address of the rom file"
	 (*"-raminspect", Arg.List (fun l -> options.ramlist <- l),
				 "IntList of the ram addresses to follow";*)
				 ]
   
let main_exec filename= 
    let netp = Netlist.read_file filename in
    let out_name = (Filename.chop_suffix filename ".net") ^ "_sch.net" in
    let out = open_out out_name in
    let close_all () =
      close_out out
    in
    begin try
        let p = Scheduler.schedule netp in
		if options.oprint then
			Netlist_printer.print_program out p; 
			(* Netlist_printer est a modifier pour indiquer comment est traite le cas des registres *)
		if not options.oschedule then (
			let mp = conversion_programme p in
			begin try 
			 mp.mp_tabram <- Dataio.read_ram options; 
			with Sys_error _ -> Format.eprintf "no ram loaded\n"
			end ;
			begin try
			 mp.mp_tabrom <- Dataio.read_rom options;
			with Sys_error _ -> Format.eprintf "no rom loaded\n"
			end ;

				 if options.odebug then
				  Execution.exec_debug mp options 
				else
				  Execution.execution  mp options 
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
