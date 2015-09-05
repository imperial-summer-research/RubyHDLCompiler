(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This file contains the toplevel "rc" function.                  ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Main :
  sig
(*      val rcomp : string -> unit    allow access with SML env *)
        (* tjt97: was
        val rc    : (string list * string list) -> unit
        val rcHandle : string list -> unit
		*)
        val rc    : (string * string list) -> int
        val rcHandle : (string list * string list) -> unit
  end =
struct

(**************************************************************************)
(**************************************************************************)

val genALX = ref false;
val genBLK = ref false;
val genXNF = ref false;
val genEDF = ref false;
val genVHD = ref false;
(* tjt: add Verilog as "VLG" *)
val genVLG = ref false;
(* tjt: add Maxeler MaxJ as "MAX" *)
val genMAX = ref false;

(**************************************************************************)
(***                                                                    ***)
(***    rcomp deals with file i/o, and calls the parsing, compiling,    ***)
(***    and TextIO.output formatting functions:                                ***)
(***                                                                    ***)
(**************************************************************************)

(* tjt: add commandLineOptions, spacedCommandLine 
            commandLineOptions to easily extract options
			spacedCommandLine to print the original command line to file
 *)

fun rcomp file commandLineOptions spacedCommandLine

(**************************************************************************)
(***    parse the toplevel file into defs:                              ***)

= let  val defs = (State.SourceFiles := [file];
                   ruby.parse file)

(**************************************************************************)
(***    find the current name:                                          ***)

       fun firstDefName [] = Errors.simple_error ("no current name found in " ^ file)
         | firstDefName (Rubytype.Fdefn (f,_,_)::ds) = f
         | firstDefName (_            ::ds) = firstDefName ds        
       val current = firstDefName defs

(**************************************************************************)
(***    compile toplevel file into symbol tables:                       ***)

       val dummy = app CompileDef.storeDef defs

(**************************************************************************)
(***    compile all "include" files into symbol tables:                 ***)

       val dummy = while  not (null (!State.IncludeFiles))
                   do     let  val incl = hd (!State.IncludeFiles)
                          in   State.IncludeFiles := tl (!State.IncludeFiles);
                               if    Utilities.notElem incl (!State.SourceFiles)
                               then  (State.SourceFiles := incl :: (!State.SourceFiles);
                                      app CompileDef.storeDef (ruby.parse incl))
                               else  ()
                          end

(**************************************************************************)
(***    find the value of the current name:                             ***)

       val dummy = State.CurrentPrim := "rc"
       val currentValue = CompileDef.globalDef current

(**************************************************************************)
(***    generate a circuit from the current name:                       ***)

       val circuit = Values.value2circuit currentValue

(**************************************************************************)
(***    organise the gates into parallel blocks:                        ***)

       val pcircuit = Order.circuit2pcircuit circuit

(**************************************************************************)
(***    write the rbs version of the pcircuit:                          ***)

       val outfile = TextIO.openOut (current^".rbs")
       val dummy = TextIO.output (outfile, Printrbs.showRBS pcircuit)
       val dummy = TextIO.closeOut outfile

(**************************************************************************)
(***    possibly write other versions of the pcircuit:                  ***)

  in   if    !genALX
       then  let  val outfile = TextIO.openOut (current^".cfg")
             in   (TextIO.output (outfile, Printalx.showALX current pcircuit);
                   TextIO.closeOut outfile)
             end
       else  ();

       if    !genBLK
       then  let  val outfile = TextIO.openOut (current^".blk")
             in   (TextIO.output (outfile, Printblk.showBLK pcircuit);
                   TextIO.closeOut outfile)
             end
       else  ();

       if    !genEDF
       then  let  val outfile = TextIO.openOut (current^".edn")
             in   (TextIO.output (outfile, Printedf.showEDF pcircuit);
                   TextIO.closeOut outfile)
             end
       else  ();

       if    !genVHD
       then  let  val outfile = TextIO.openOut (current^".vhd")
             in   (TextIO.output (outfile, PrintVhdl.printVHDL pcircuit);
                   TextIO.closeOut outfile)
             end
       else  ();
      
	  (* tjt: Verilog *)
	   if    !genVLG
       then  let  val outfile = TextIO.openOut (current^".v")
             in   (TextIO.output (outfile, PrintVerilog.printVerilog pcircuit);
                   TextIO.closeOut outfile)
             end
       else  ();

		(* tjt: Maxeler MaxJ *)
    (* vincent: add support for multiple kernels generation *)
	   if    !genMAX
       then let  
              val (kernelList, manager) = MaxGraph.splitKernel pcircuit
            in   
            ( List.app 
              ( fn (name,pc) => 
                ( let val outFile  = TextIO.openOut (name^".maxj") in 
                  ( TextIO.output 
                      ( outFile, 
                        PrintMax.showMax name pc commandLineOptions
                        spacedCommandLine);
                    TextIO.closeOut outFile)
                  end ))
              kernelList;
              ( let val outFile = TextIO.openOut "Manager.maxj" in
                  ( TextIO.output 
                    ( outFile, 
                      PrintMax.showMaxManager "Manager0" manager);
                    TextIO.closeOut outFile) 
                end ))
			 	(*
                (TextIO.output (outKernelFile, 
				    PrintMax.showMax current pcircuit commandLineOptions
					spacedCommandLine);
                    TextIO.closeOut outKernelFile)
                
                   (TextIO.output (outManagerFile, 
                      PrintMax.showMaxManager current pcircuit commandLineOptions
                        spacedCommandLine);
                   TextIO.closeOut outManagerFile))
                *)
             end
       else  ();


       if    !genXNF
       then  let  val outfile = TextIO.openOut (current^".xnf")
             in   (TextIO.output (outfile, Printxnf.showXNF pcircuit);
                   TextIO.closeOut outfile)
             end
       else  ()

  end


(* tjt: Utility to print lists: each item surrounded by single quotes, terminated by
commas
*)
fun print_list (args : string list)
	= (
		print "{";
		map (fn x => (print "'"; print x; print "',")) args;
		print "}";
		0
	)
 
(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    rc is the toplevel function to be exported to the Unix          ***)
(***    environment:                                                    ***)
(***                                                                    ***)
(**************************************************************************)

(* tjt97: was:
fun rcHandle argv
*)
fun rcHandle (argv, envp)
        = let 
		       (* tjt: define: flags as two characters, beginning with '-' 
			    *              options as anything beginning with '-'
				*              files as anything else
			   *)
               fun isFlagOrOption x = (str (hd (explode x)) = "-")
		       fun isFlag x = (isFlagOrOption(x)) andalso (size(x) = 2)
               (* was
			   val flags = Utilities.filter isFlag argv *)
               val flags = Utilities.filter isFlag envp
               val flags' = Utilities.filter (fn c => c<>("-")) (map str (Utilities.flatmap explode flags))
			   (* was
               val files = Utilities.filter (not o isFlag) argv *)
               val files = Utilities.filter (not o isFlagOrOption) envp

               fun setFlag "a" = genALX := true
                 | setFlag "b" = genBLK := true
                 | setFlag "x" = genXNF := true
                 | setFlag "v" = genVHD := true
				 (* tjt: use "-l" for verilog, as VHDL already uses "-v" *)
                 | setFlag "l" = genVLG := true
                 | setFlag "e" = genEDF := true
                 | setFlag "m" = genMAX := true
                 | setFlag  f  = Errors.usage_error ("ERROR: " ^ f ^ ": unknown flag")
				 

		       (* tjt: command line as list of chars, spaced with single space
			   between each item *)
			   val spacedCommandLine = 
			      (explode o concat o CommandLineParse.intersperse " ") envp
			   (* tjt: parse command line into defs *)
			   val commandLineOptions =
			      CommandLineParse.fst(CommandLineParse.parseCL([],
				     CommandLineParse.lexCL spacedCommandLine))

		(* tjt: Ruby has only ever supported single input file *)
          in   if    length files < 1  (* tjt97: was 2 in old SML; first arg is no longer command name *)
               then  (
			          (* print ("len files = " ^ Int.toString (length files) ^ "\n"); *)
			          print "files = ";
					  print_list files;
			          Errors.usage_error "ERROR: no input file"
			         )
               else if    length files > 1  (* tjt97: was 2 in old SML; first arg is no longer command name *)
               then  (print ("len files = " ^ Int.toString (length files) ^ "\n");
			          print "files = ";
					  print_list files;
			          Errors.usage_error "ERROR: only one input file supported"
			         )
               else  (
			          print "files = ";
					  print_list files;
					  print "\nflags' = ";
					  print_list flags';

					  (* command line input to command line parser *)
					  print "\nspaced command line = {";
					  print (implode spacedCommandLine);
					  print "}\n";
					  (*
					  map print flags';
					  *)
					  print "flags: {";
					  print_list flags;
					  print "}\n";
			          app setFlag flags';
					  (* was
                      rcomp (hd (tl files)))
					  *)
					  print "before compile\n";
                      rcomp (hd files) commandLineOptions spacedCommandLine);
					  print "after compile\n";

					  print ""   (* last stmt must not end in semciolon *)
          end
		  (*
		  tjt: removed exception handler from here
		  *)



(**************************************************************************)

(* tjt: was
fun rc (argv:string list, envp:string list)
        = rcHandle argv
also add banner "Ruby compiler"
*)
fun rc (argv:string, envp:string list)
        = (
		   print ("Imperial Ruby compiler (vzhao)\n");
		   print ("argv = " ^ argv ^ "\n");
		   print "envp = {";
		   (*
		   map (fn x => (print "'"; print x; print "',")) envp;
		   *)
		   print_list envp;
		   print "}\n";
		   (*
		   ** for some reason we can't print arguments like this
		   print "cl args = ";
		   map (fn x => (print x; print " ")) CommandLine.arguments();
		   print "\n";
		   *)
		   rcHandle (String.tokens Char.isSpace argv, envp); 
		   0    (*exit code 0: success! *)
		   )
		   (*tjt: put exception handler here so can return exit code
		          helps make test to stop on 1st error
		   *)
          handle ex => 
		  (
		  	TextIO.output (TextIO.stdErr, "\n" ^ Messages.message ex ^ "\n\n");
			127 (*exit code 127: error*)
			)

(**************************************************************************)
(**************************************************************************)

end (* of structure Main *);
(* open Main *)
