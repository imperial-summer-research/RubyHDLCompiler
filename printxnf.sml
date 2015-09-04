(* 14/7/95/ for the spectrum system, revise the pin name
   output 
   further change about showGate on 23/1/96 
 *)





(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These functions give an XNF implementation of a circuit:        ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Printxnf :
  sig
        val showXNF : Circuittype.pcircuit -> string
  end =
struct

(**************************************************************************)
(**************************************************************************)


fun showDev Circuittype.NOT  = "INV"
  | showDev Circuittype.AND  = "AND"
  | showDev Circuittype.OR   = "OR"
  | showDev Circuittype.XOR  = "XOR"
  | showDev Circuittype.NAND = "NAND"
  | showDev Circuittype.NOR  = "NOR"
  | showDev Circuittype.XNOR = "XNOR"
  | showDev d    = Errors.simple_error ("device \"" ^ Show.showDevice d ^
                                 "\" not allowed in XNF format")

fun showWire (Circuittype.CON (Rubytype.BOOL false)) = "GND"
  | showWire (Circuittype.CON (Rubytype.BOOL true))  = "VCC"
  | showWire (Circuittype.CON (Rubytype.INT 0))      = "GND"
  | showWire (Circuittype.CON (Rubytype.INT 1))      = "VCC"
  | showWire (Circuittype.CON (Rubytype.SYM "?"))    = "GND"
  | showWire (Circuittype.CON k)            = Errors.simple_error("constant \"" ^ Show.showConst k ^
                                               "\" not allowed in XNF format")
  | showWire (Circuittype.WIRE (_,n,_))     = "ruby_w" ^ Int.toString (n:int)

fun showIo (Rubytype.CONST k) = showWire (Circuittype.CON k)
  | showIo (Rubytype.VAR s)   = s

fun showLatch (input, output)
        = let  val outStr = showWire (hd (Exprs.flattenExpr output))
               val inStr  = showWire (hd (Exprs.flattenExpr input))
               val ceStr  = if    (!Data.DATA_xnf_clk_ena) = ""
                            then  ""
                            else  "PIN,CE,I," ^ !Data.DATA_xnf_clk_ena ^ "\n"
          in   "SYM,D_" ^ outStr ^ ",DFF\n" ^
               "PIN,Q,O," ^ outStr        ^ "\n" ^
               "PIN,D,I," ^ inStr         ^ "\n" ^
               "PIN,C,I," ^ !Data.DATA_xnf_clk ^ "\n" ^
               ceStr ^
               "END\n"
          end

fun showPin (w,i) = "PIN," ^ Int.toString (i:int) ^ ",I," ^ w ^ "\n"



fun showGate (Circuittype.D,               input, output) = showLatch (input, output)
  | showGate (Circuittype.DI (Rubytype.BOOL False), input, output) = showLatch (input, output)
  | showGate (Circuittype.DI (Rubytype.INT 0),      input, output) = showLatch (input, output)
  | showGate (Circuittype.DI (Rubytype.SYM "?"),    input, output) = showLatch (input, output)
  | showGate (dev,             input, output)
        = let  val devStr = showDev dev
               val outStr = showWire (hd (Exprs.flattenExpr output))
               val inStrs = map showWire (Exprs.flattenExpr input)
          in   if devStr = "INV"
                 then "SYM,INV_" ^ outStr ^ "," ^ devStr ^ "\n" ^
                      "PIN,O,O," ^ outStr ^ "\n" ^ "PIN,I,I," ^
                       hd inStrs ^ "\n" ^ "END\n"
                 else "SYM," ^ devStr ^ "_" ^ outStr ^ "," ^ devStr ^ "\n" ^
                      "PIN,O,O," ^ outStr ^ "\n" ^
                       Utilities.concatWith "" (map showPin 
                         (Utilities.pairWithIntsFrom 2 (rev inStrs))) ^
                      "END\n"
          end

(* further changed into the above on 23/1/96 *)

(*
fun showGate (D,               input, output) = showLatch (input, output)
  | showGate (DI (Rubytype.BOOL False), input, output) = showLatch (input, output)
  | showGate (DI (Rubytype.INT 0),      input, output) = showLatch (input, output)
  | showGate (DI (SYM "?"),    input, output) = showLatch (input, output)
  | showGate (dev,             input, output)
        = let  val devStr = showDev dev
               val outStr = showWire (hd (Exprs.flattenExpr output))
               val inStrs = map showWire (Exprs.flattenExpr input)
          in   "SYM," ^ devStr ^ "_" ^ outStr ^ "," ^ devStr ^ "\n" ^
               "PIN,O,O," ^ outStr ^ "\n" ^
               concatWith "" (map showPin (pairWithIntsFrom 2 (rev inStrs))) ^
               "END\n"
          end
*)



(* changed into the above on 14/7/95/ *)
(* for Spectrum, the names of the input pins start from 2*)

(*
fun showGate (D,               input, output) = showLatch (input, output)
  | showGate (DI (Rubytype.BOOL False), input, output) = showLatch (input, output)
  | showGate (DI (Rubytype.INT 0),      input, output) = showLatch (input, output)
  | showGate (DI (SYM "?"),    input, output) = showLatch (input, output)
  | showGate (dev,             input, output)
        = let  val devStr = showDev dev
               val outStr = showWire (hd (Exprs.flattenExpr output))
               val inStrs = map showWire (Exprs.flattenExpr input)
          in   "SYM," ^ devStr ^ "_" ^ outStr ^ "," ^ devStr ^ "\n" ^
               "PIN,O,O," ^ outStr ^ "\n" ^
               concatWith "" (map showPin (pairWithIntsFrom 1 (rev inStrs))) ^
               "END\n"
          end
*)

fun showIbuf (w, s)
        = "SYM,ibuf_" ^ s ^ ",BUF\n" ^
          "PIN,O,O," ^ showWire w ^ "\n" ^
          "PIN,I,I," ^ s ^ "\n" ^
          "END\n"

fun splitName n = let  val n' = explode n
                       val loc = Utilities.takewhile (fn x => x<>(#"_")) n'
                       val name = Utilities.drop (length loc + 1, n')
                       val nameblock = if    name = []
                                       then  ""
                                       else  ",BLKNM=" ^ implode name
                  in   (implode loc, nameblock)
                  end

fun showIBbuf (w, s)
        = let  val (loc, nameblock) = splitName s
          in   "SYM,xibuf_" ^ loc ^ ",IBUF\n" ^
               "PIN,O,O," ^ showWire w ^ "\n" ^
               "PIN,I,I," ^ loc ^ "\n" ^
               "END\n" ^
               "EXT," ^ loc ^ ",I,,LOC=" ^ loc ^ nameblock ^ "\n"
          end

fun showObuf (w, s)
        = "SYM,obuf_" ^ s ^ ",BUF\n" ^
          "PIN,O,O," ^ s ^ "\n" ^
          "PIN,I,I," ^ showWire w ^ "\n" ^
          "END\n"

fun showOBbuf (w, s)
        = let  val (loc, nameblock) = splitName s
          in   "SYM,xobuf_" ^ loc ^ ",OBUF\n" ^
               "PIN,O,O," ^ loc ^ "\n" ^
               "PIN,I,I," ^ showWire w ^ "\n" ^
               "END\n" ^
               "EXT," ^ loc ^ ",O,,LOC=" ^ loc ^ nameblock ^ "\n"
          end

fun showOBTbuf (w, s)
        = let  val oe = if    !Data.DATA_xnf_out_ena = ""
                        then  Errors.simple_error ("&xnf_out_ena not defined")
                        else  !Data.DATA_xnf_out_ena
               val (loc, nameblock) = splitName s
          in   "SYM,xobuf_" ^ loc ^ ",OBUFT\n" ^
               "PIN,O,O," ^ loc ^ "\n" ^
               "PIN,I,I," ^ showWire w ^ "\n" ^
               "PIN,T,I," ^ oe ^ "\n" ^
               "END\n" ^
               "EXT," ^ loc ^ ",O,,LOC=" ^ loc ^ nameblock ^ "\n"
          end

fun showConnexion (w, Rubytype.VAR x)
        = (case explode x of
             (#"i")::(#"_")::loc           => showIbuf  (w, implode loc)
           | (#"i")::(#"b")::(#"_")::loc      => showIBbuf (w, implode loc)
           | (#"o")::(#"_")::loc           => showObuf  (w, implode loc)
           | (#"o")::(#"b")::(#"_")::loc      => showOBbuf (w, implode loc)
           | (#"o")::(#"b")::(#"t")::(#"_")::loc => showOBTbuf(w, implode loc)
           | s                       => Errors.simple_error ("invalid IO name: \"" ^
                                                      implode s ^ "\"")
          )
  | showConnexion (w, d)
        = showIbuf (w, showIo d)

fun showXNF (dom, ran, rels)
  = Utilities.fileContents (!Data.DATA_xnf_header) ^
    Utilities.concatWith ""
           (map showGate (Utilities.concat rels) @
            map showConnexion (IoAlias.ioAlias("xnf_domain",dom,!Data.DATA_xnf_domain_io)) @
            map showConnexion (IoAlias.ioAlias("xnf_range",ran,!Data.DATA_xnf_range_io))) ^
    "EOF\n"

(**************************************************************************)
(**************************************************************************)

end (* of structure Printxnf *);
(* open Printxnf *)
