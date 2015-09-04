(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These functions give an ALX implementation of a pcircuit:       ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Printalx :
  sig
        val showALX : string -> Circuittype.pcircuit -> string
  end =
struct

(**************************************************************************)
(**************************************************************************)

fun showDev Circuittype.NOT                 = "X1BAR"
  | showDev Circuittype.AND                 = "AND"
  | showDev Circuittype.OR                  = "OR"
  | showDev Circuittype.XOR                 = "XOR"
  | showDev Circuittype.NAND                = "NAND"
  | showDev Circuittype.NOR                 = "NOR"
  | showDev Circuittype.XNOR                = "XNOR"
  | showDev (Circuittype.DEVICE d)          = d
  | showDev d                   = Errors.simple_error ("device \"" ^ Show.showDevice d ^
                                                "\" not allowed in ALX format")

val clock = (Circuittype.OUT,0,0)

fun showWire (_,0,_)    = !Data.DATA_alx_clk
  | showWire (_,n,_)    = "ruby_w" ^ Int.toString (n:int)

fun showInput (i,n)     = "\n    X" ^ Int.toString (n:int) ^ "NET " ^ showWire i

fun showCell ((dev,ins,outs), pos)
        = let  val pos' = Int.toString (pos:int)
          in   "  CELL rubycell_" ^ pos' ^ " " ^ pos' ^ " " ^ pos' ^
               Utilities.concatWith "" (map showInput (Utilities.pairWithIntsFrom 1 ins)) ^
               "\n    SELFNET " ^ showWire (hd outs) ^
               "\n    FUNCTION " ^
                showDev dev ^
               "\n  ENDCELL\n"
          end

fun showCells gs = Utilities.concatWith "" (map showCell (Utilities.pairWithIntsFrom 0 gs))

fun pairWithDirections []
        = []
  | pairWithDirections (w::ws)
        = let  val dir = (case w of
                            (Circuittype.POLY,_,_)  => if    Utilities.elem w ws
                                           then  Circuittype.OUT
                                           else  Circuittype.IN
                          | (dir ,_,_)  => dir
                         )
          in   (w,dir) :: pairWithDirections ws
          end

fun showDir Circuittype.IN  = "IN"
  | showDir Circuittype.OUT = "OUT"

fun showRport ((w,dir),pos) = "  PORT " ^ showDir dir ^
                          " " ^ showWire w ^ " " ^
                          Int.toString (pos:int) ^ " 0 SOUTH"

fun showDport ((w,dir),pos) = "  PORT " ^ showDir dir ^
                          " " ^ showWire w ^ " 0 " ^
                          Int.toString (pos:int) ^ " WEST"

fun showPorts (ds, rs) = let val drs = rev (pairWithDirections (rev (ds@rs)))
                             val ds' = Utilities.pairWithIntsFrom 0 (Utilities.take(length ds, drs))
                             val rs' = Utilities.pairWithIntsFrom 0 (Utilities.drop(length ds, drs))
                         in  Utilities.concatWith "\n" (map showRport rs' @
                                              map showDport ds') ^
                             "\n  ENDPORTS\n"
                         end

fun const_error k = Errors.simple_error ("constant \"" ^ Show.showConst k ^
                                  "\" not allowed in ALX format")

fun const2gate (Rubytype.BOOL false) = "ZERO"
  | const2gate (Rubytype.BOOL true)  = "ONE"
  | const2gate (Rubytype.INT 0)      = "ZERO"
  | const2gate (Rubytype.INT 1)      = "ONE"
  | const2gate k            = const_error k

fun expr2wires e = (case !e of
                      Circuittype.CON k    => let  val Circuittype.WIRE w = !(Exprs.monoOut 0)
                                  in   ([w], [(Circuittype.DEVICE (const2gate k), [], [w])])
                                  end
                    | Circuittype.WIRE w   => ([w], [])
                    | Circuittype.EXPR x   => expr2wires x
                    | Circuittype.LIST es  => let  val (ws,cs) = Utilities.unzip (map expr2wires es)
					(* tjt, Jan 2014: note we must use our Utilities.concat,
					not the standard concat, or ther are type errors *)
                                  in   (Utilities.concat ws, Utilities.concat cs)
                                  end
                   )

fun latchPair (inputs, outputs)
        = let  val Circuittype.WIRE w = !(Exprs.monoOut 0)
          in   [(Circuittype.DEVICE "DLATCH",    clock::inputs, [w]),
                (Circuittype.DEVICE "CBARLATCH", [clock,w],     outputs)]
          end


fun convertLatch (Circuittype.D,    input, output) = latchPair (input, output)
  (* tjt: was
  | convertLatch (DI k, input, output) = if    k = (BOOL false)  orelse
                                               k = (INT 0)       orelse
                                               k = (SYM "?")
                                         then  latchPair (input, output)
                                         else  const_error k
										 *)
  | convertLatch (Circuittype.DI (Rubytype.BOOL false), input, output) = latchPair (input, output)
  | convertLatch (Circuittype.DI (Rubytype.BOOL true), input, output) = const_error (Rubytype.BOOL false)
  | convertLatch (Circuittype.DI (Rubytype.INT k), input, output) = (if k = 0 then latchPair (input, output) else const_error (Rubytype.INT k))
  | convertLatch (Circuittype.DI (Rubytype.SYM k), input, output) = (if k = "?" then latchPair (input, output) else const_error (Rubytype.SYM k))
  | convertLatch (Circuittype.DI (Rubytype.REAL r), input, output) = const_error (Rubytype.REAL r)
  | convertLatch g                     = [g]

fun convertConsts (dev,input,output)
        = let  val (input',  iconsts) = expr2wires input
               val (output', oconsts) = expr2wires output
          in   iconsts @ oconsts @ [(dev, input', output')]
          end

(**************************************************************************)
(***                                                                    ***)
(***    In this format, constants must be converted into wires and      ***)
(***    latches must be converted into DLATCH, CBARLATCH pairs:         ***)
(***                                                                    ***)
(**************************************************************************)

fun showALX blockname (dom, ran, rels)
        = let  val (dwires, dconsts) = expr2wires dom
               val (rwires, rconsts) = expr2wires ran
			   (*tjt, Jan 2014: must use Utilties.concat, not standard concat
			   or type errors *)
               val rel'  = Utilities.flatmap convertConsts (Utilities.concat rels)
               val rel'' = Utilities.flatmap convertLatch rel'
          in   "BLOCK " ^ blockname ^ "\n" ^
               showPorts (dwires, rwires) ^
               showCells (dconsts @ rel'' @ rconsts) ^
               "ENDBLOCK\n\nENDFILE\n"
          end

(**************************************************************************)
(**************************************************************************)

end (* of structure Printalx *);
(* open Printalx *)
