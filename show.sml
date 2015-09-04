(* 16/8/95. change MUX => MUXR, for the output
 *)





(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Functions for displaying basic values in circuits:              ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Show :
  sig
        val showConst  : Rubytype.const -> string
        val showDevice : Circuittype.device -> string
  end =
struct

(**************************************************************************)
(**************************************************************************)

fun showConst (Rubytype.BOOL true)        = "T"
  | showConst (Rubytype.BOOL false)       = "F"
  | showConst (Rubytype.INT i)            = Int.toString i
  | showConst (Rubytype.REAL n)           = Real.toString n
  | showConst (Rubytype.SYM s)            = s

fun showDevice Circuittype.D                 = "D ?"
  | showDevice (Circuittype.DI k)            = "D "    ^ showConst k
  | showDevice (Circuittype.MUX n)           = "MUXR "  ^ Int.toString n
  | showDevice (Circuittype.SDPR n)          = "sdpr " ^ Int.toString n
  | showDevice (Circuittype.PDSR n)          = "pdsr " ^ Int.toString n
  | showDevice Circuittype.NOT               = "not"
  | showDevice Circuittype.AND               = "and"
  | showDevice Circuittype.OR                = "or"
  | showDevice Circuittype.XOR               = "xor"
  | showDevice Circuittype.NAND              = "nand"
  | showDevice Circuittype.NOR               = "nor"
  | showDevice Circuittype.XNOR              = "xnor"
  | showDevice Circuittype.LT                = "ltn"
  | showDevice Circuittype.GT                = "gtn"
  | showDevice Circuittype.LE                = "leq"
  | showDevice Circuittype.GE                = "geq"
  | showDevice Circuittype.EQ                = "eq"
  | showDevice Circuittype.IF                = "if"
  | showDevice Circuittype.BTOI              = "bit2int"
  | showDevice Circuittype.ITOB              = "int2bit"
  | showDevice Circuittype.ADD               = "add"
  | showDevice Circuittype.SUB               = "minus"
  | showDevice Circuittype.MULT              = "mult"
  | showDevice Circuittype.DIV               = "div"
  | showDevice Circuittype.MOD               = "mod"
  | showDevice Circuittype.EXP               = "exp"
  | showDevice Circuittype.LOG               = "log"
  | showDevice Circuittype.MAX               = "max"
  | showDevice Circuittype.MIN               = "min"
  | showDevice Circuittype.GCD               = "gcd"
  | showDevice Circuittype.FAC               = "fac"
  | showDevice Circuittype.DEC               = "dec"
  | showDevice Circuittype.INC               = "inc"
  | showDevice Circuittype.AD                = "AD"
  | showDevice Circuittype.ABS               = "abs"
  | showDevice Circuittype.REAL2INT          = "real2int"
  | showDevice Circuittype.INT2REAL          = "int2real"
  | showDevice (Circuittype.BIT2UINT n)      = "bit2uint "
  | showDevice (Circuittype.BIT2SINT n)      = "bit2sint "
  | showDevice (Circuittype.UINT2BIT n)      = "uint2bit " ^ Int.toString n
  | showDevice (Circuittype.SINT2BIT n)      = "sint2bit " ^ Int.toString n
  | showDevice (Circuittype.BIT2UREAL (m,n)) = "bit2ureal "
  | showDevice (Circuittype.BIT2SREAL (m,n)) = "bit2sreal "
  | showDevice (Circuittype.UREAL2BIT (m,n)) = "ureal2bit " ^ Int.toString m ^ " " ^ Int.toString n
  | showDevice (Circuittype.SREAL2BIT (m,n)) = "sreal2bit " ^ Int.toString m ^ " " ^ Int.toString n

(**************************************************************************)
(**************************************************************************)

end (* of structure Show *);
(* open Show *)
