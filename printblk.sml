(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These functions give a BLK implementation of a pcircuit:        ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Printblk :
  sig
        val showBLK : Circuittype.pcircuit -> string
  end =
struct

(**************************************************************************)
(**************************************************************************)

fun showDev Circuittype.NOT = "INV"
  | showDev Circuittype.AND = "AND"
  | showDev Circuittype.OR  = "OR"
  | showDev Circuittype.XOR = "XOR"
  | showDev d   = Errors.simple_error ("device \"" ^ Show.showDevice d ^
                                "\" not allowed in BLK format")

fun showWire w = "sig_of \"" ^
                 (case w of
                    (Circuittype.CON (Rubytype.BOOL false)) => "GND"
                  | (Circuittype.CON (Rubytype.BOOL true))  => "VCC"
                  | (Circuittype.CON (Rubytype.INT 0))      => "GND"
                  | (Circuittype.CON (Rubytype.INT 1))      => "VCC"
                  | (Circuittype.CON (Rubytype.SYM "?"))    => "GND"
                  | (Circuittype.CON k)            => Errors.simple_error ("constant \"" ^
                                          Show.showConst k ^
                                          "\" not allowed in BLK format")
                  | (Circuittype.WIRE (_,n,_))     => "ruby_w" ^ Int.toString (n:int)
                 ) ^ "\""

fun showIo (Rubytype.VAR s)    = "sig_of \"" ^ s ^ "\""
  | showIo (Rubytype.CONST k)  = showWire (Circuittype.CON k)

fun showLatch (input, output)
        = "DType ("      ^ showWire (hd (Exprs.flattenExpr output)) ^
          ", "           ^ showWire (hd (Exprs.flattenExpr  input)) ^
          ", sig_of \""  ^ (!Data.DATA_blk_clk_ena) ^ "\"" ^
          ", sig_of \"GND\"" ^
          ", sig_of \""  ^ (!Data.DATA_blk_clk) ^ "\")"

fun showGate (Circuittype.D,               input, output) = showLatch (input, output)
  | showGate (Circuittype.DI (Rubytype.BOOL False), input, output) = showLatch (input, output)
  | showGate (Circuittype.DI (Rubytype.INT 0),      input, output) = showLatch (input, output)
  | showGate (Circuittype.DI (Rubytype.SYM "?"),    input, output) = showLatch (input, output)
  | showGate (dev,             input, output)
        = "Gate (" ^ showDev dev ^
          ", "     ^ showWire (hd (Exprs.flattenExpr output)) ^
          ", ["    ^ Utilities.concatWith ", " (map showWire (Exprs.flattenExpr input)) ^ "])"

fun showConnexion (w, io) = "Wire [" ^ showWire w ^ ", " ^ showIo io ^ "]"

fun showBLK (dom, ran, rels)
  = "\nBlockList := [\n" ^
    Utilities.concatWith ",\n"
               (map showGate (Utilities.concat rels) @
                map showConnexion (IoAlias.ioAlias("domain",dom,!Data.DATA_blk_domain_io)) @
                map showConnexion (IoAlias.ioAlias("range",ran,!Data.DATA_blk_range_io))) ^
    "\n] @ (!BlockList) ;\n"

(**************************************************************************)
(**************************************************************************)

end (* of structure Printblk *);
(* open Printblk *)
