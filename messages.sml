(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Output formatting for error messages:                           ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Messages:
  sig
        val message : exn -> string
  end =
struct

(**************************************************************************)
(**************************************************************************)

(* tjt97 - makestring => Int.toString 
 * Io => IO.Io, now returns some sort of record
 *
 * Jan 2014: TextPosition => Errors.TextPosition
 *           SourceFiles => State.SourceFiles
 *)
fun showPosition () = let val (l,c) = !Errors.TextPosition
                      in  ", line " ^ Int.toString l ^ ", column " ^ Int.toString c
                      end

fun showFile () = " in file \"" ^ hd (!State.SourceFiles) ^ "\""

(* Jan 2014: concatWith => Utilities.concatWith
 *           FunStack => State.FunStack   (I must get one of those; sounds
 *                                         great!)
 *)
fun showFunctions () = " in " ^ Utilities.concatWith "->" (rev (!State.FunStack))

(**************************************************************************)

(* tjt, Jan 2014: SIMPLE_ERROR => Errors.SIMPLE_ERROR, etc. *)
fun message (Errors.SIMPLE_ERROR s) = "Error: " ^ s

  | message (Errors.LEX_ERROR s)    = "Bad character \"" ^ s ^ "\"" ^
                               showFile () ^ showPosition ()

  | message (Errors.PARSE_ERROR s)  = "Syntax error at \"" ^ s ^ "\"" ^
                               showFile () ^ showPosition ()

  | message (Errors.SEQ_ERROR s)    = "Sequential composition error" ^
                               showFunctions () ^ showPosition () ^
                               ":\n" ^  s

  | message (Errors.DEF_ERROR s)    = "Error" ^ showFunctions () ^ ":\n" ^ s

  | message (Errors.LOOP_ERROR ds)  = "Unbroken loop error:\n{" ^
                               (* tjt *)
                               Utilities.concatWith "," (rev ds) ^
                               "}"

  | message (Errors.USAGE_ERROR s)  = s ^ "\nusage: rc [-abxevlm] file" ^

                                   "\n   -m   output Maxeler MaxJ codes to file current.maxj" ^
                                   "\n   -v   output VHDL codes to file current.vhd (experimental)" ^ 
                                   "\n   -l   output Verilog codes to file current.v (experimental)" ^
                                   "\n   -e   output EDIF codes to file current.edn"  ^
                                   "\n   -a   output a file current.alx for Algotronix's FPGAs (deprecated)" ^
                                   "\n   -b   output an BLK file current.blk (deprecated)" ^
                                   "\n   -x   output an xnf file current.xnf (deprecated)" ^
								   ""


  | message (IO.Io {cause, function, name})           = "Error: " ^ name ^ " in: " ^ function
                               (*raised by SML standard library file handling*)

(**************************************************************************)
(**************************************************************************)

end (* of structure Messages *);
(* open Messages *)
