(* pas d'open necessaires pour le main à part Def *)
(* definition des options *)
open Def
				
let options = {oprint = true; oschedule = false; odebug = false; 
			   overbose = false; osteps = -1; oclock = 1.; 
			   ramsize=256; romsize=256; ramlist = [] ;
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
                 "print all the outputs"
	 "-ramsize", Arg.Int  (fun x -> options.ramsize <- x),
				 "Size (in word) of the ram array";
	 "-romsize", Arg.Int  (fun x -> options.ramsize <- x),
				 "Size (in word) of the rom array";
	 "-raminspect", Arg.List (fun l -> options.ramlist <- l),
				 "IntList of the ram adresses to follow"
	 "-ramfile", Arg.String (fun s -> options.ram_file <- s)
				 "adress of the ram file"
	 "-romfile", Arg.String (fun s -> options.ram_file <- s)
				 "adress of the rom file"
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
			(* Netlist_printer est à modifier pour indiquer comment est traité le cas des registres *)
		if not options.oschedule then (
			let ram = Dataio.readram options in
			let rom = Dataio.readrom options in
			let mp = conversion_programme p in
				 if options.odebug then
				  Execution.exec_debug mp options ram rom  
				else
				  Execution.execution  mp options ram rom
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
