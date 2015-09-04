(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Implementation of built-in arithmetic primitives:               ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure ArithPrims :
  sig
        val arithNeg    : Rubytype.const           -> Values.value
        val arithPlus   : (Rubytype.const * Rubytype.const) -> Values.value
        val arithMinus  : (Rubytype.const * Rubytype.const) -> Values.value
        val arithTimes  : (Rubytype.const * Rubytype.const) -> Values.value
        val arithDivide : (Rubytype.const * Rubytype.const) -> Values.value
        val arithMax    : (Rubytype.const * Rubytype.const) -> Values.value
        val arithMin    : (Rubytype.const * Rubytype.const) -> Values.value
        val arithMod    : (int   *   int) -> Values.value
        val arithGtn    : (Rubytype.const * Rubytype.const) -> Values.value
        val arithGeq    : (Rubytype.const * Rubytype.const) -> Values.value
        val arithLtn    : (Rubytype.const * Rubytype.const) -> Values.value
        val arithLeq    : (Rubytype.const * Rubytype.const) -> Values.value
        val arithEq     : (Rubytype.const * Rubytype.const) -> Values.value
        val arithNot    : int             -> Values.value
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These primitives implement simple arithmetic in the source      ***)
(***    language.  They are not used for creating circuits; thus they   ***)
(***    return circuits which represent constant values:                ***)
(***                                                                    ***)
(**************************************************************************)

fun intValue  i = Values.CONSTANT (Rubytype.INT i)
fun realValue i = Values.CONSTANT (Rubytype.REAL i)

(**************************************************************************)

fun rmin (a:real, b:real) = if    a<=b
                            then  a
                            else  b

fun rmax (a:real, b:real) = if    a>=b
                            then  a
                            else  b

(**************************************************************************)

fun arithNeg    (Rubytype.INT n)            = intValue  (~n)
  | arithNeg    (Rubytype.REAL n)           = realValue (~n)

fun arithPlus   (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (n1      + n2)
  | arithPlus   (Rubytype.INT  n1, Rubytype.REAL n2) = realValue (real n1 + n2)
  | arithPlus   (Rubytype.REAL n1, Rubytype.INT  n2) = realValue (n1      + real n2)
  | arithPlus   (Rubytype.REAL n1, Rubytype.REAL n2) = realValue (n1      + n2)

fun arithMinus  (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (n1      - n2)
  | arithMinus  (Rubytype.INT  n1, Rubytype.REAL n2) = realValue (real n1 - n2)
  | arithMinus  (Rubytype.REAL n1, Rubytype.INT  n2) = realValue (n1      - real n2)
  | arithMinus  (Rubytype.REAL n1, Rubytype.REAL n2) = realValue (n1      - n2)

fun arithTimes  (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (n1      * n2)
  | arithTimes  (Rubytype.INT  n1, Rubytype.REAL n2) = realValue (real n1 * n2)
  | arithTimes  (Rubytype.REAL n1, Rubytype.INT  n2) = realValue (n1      * real n2)
  | arithTimes  (Rubytype.REAL n1, Rubytype.REAL n2) = realValue (n1      * n2)

fun arithDivide (_,       Rubytype.INT  0)  = Errors.def_error "division by 0"
  (* tjt97 - can't use real in pattern match
  | arithDivide (_,       Rubytype.REAL 0.0)= Errors.def_error "division by 0.0"
  *)
  | arithDivide (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (n1      div n2)
  | arithDivide (Rubytype.INT  n1, Rubytype.REAL n2) = (if Real.==(n2,0.0) then Errors.def_error "division by 0.0" else realValue (real n1 / n2))
  | arithDivide (Rubytype.REAL n1, Rubytype.INT  n2) = realValue (n1      / real n2)
  | arithDivide (Rubytype.REAL n1, Rubytype.REAL n2) = (if Real.==(n2,0.0) then Errors.def_error "division by 0.0" else realValue (n1      / n2))

fun arithMax    (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (Int.max  (n1,      n2))
  | arithMax    (Rubytype.INT  n1, Rubytype.REAL n2) = realValue (rmax (real n1, n2))
  | arithMax    (Rubytype.REAL n1, Rubytype.INT  n2) = realValue (rmax (n1,      real n2))
  | arithMax    (Rubytype.REAL n1, Rubytype.REAL n2) = realValue (rmax (n1,      n2))

fun arithMin    (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (Int.min  (n1,      n2))
  | arithMin    (Rubytype.INT  n1, Rubytype.REAL n2) = realValue (rmin (real n1, n2))
  | arithMin    (Rubytype.REAL n1, Rubytype.INT  n2) = realValue (rmin (n1,      real n2))
  | arithMin    (Rubytype.REAL n1, Rubytype.REAL n2) = realValue (rmin (n1,      n2))

fun arithMod    (_,             0) = Errors.def_error "$mod 0"
  | arithMod    (     n1,      n2) = intValue  (n1 mod n2)

fun arithGtn    (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (if n1 > n2 then 1 else 0)
  | arithGtn    (Rubytype.INT  n1, Rubytype.REAL n2) = intValue  (if real n1 > n2 then 1 else 0)
  | arithGtn    (Rubytype.REAL n1, Rubytype.INT  n2) = intValue  (if n1 > real n2 then 1 else 0)
  | arithGtn    (Rubytype.REAL n1, Rubytype.REAL n2) = intValue  (if n1 > n2 then 1 else 0)

fun arithGeq    (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (if n1 >= n2 then 1 else 0)
  | arithGeq    (Rubytype.INT  n1, Rubytype.REAL n2) = intValue  (if real n1 >= n2 then 1 else 0)
  | arithGeq    (Rubytype.REAL n1, Rubytype.INT  n2) = intValue  (if n1 >= real n2 then 1 else 0)
  | arithGeq    (Rubytype.REAL n1, Rubytype.REAL n2) = intValue  (if n1 >= n2 then 1 else 0)

fun arithLtn    (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (if n1 < n2 then 1 else 0)
  | arithLtn    (Rubytype.INT  n1, Rubytype.REAL n2) = intValue  (if real n1 < n2 then 1 else 0)
  | arithLtn    (Rubytype.REAL n1, Rubytype.INT  n2) = intValue  (if n1 < real n2 then 1 else 0)
  | arithLtn    (Rubytype.REAL n1, Rubytype.REAL n2) = intValue  (if n1 < n2 then 1 else 0)

fun arithLeq    (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (if n1 <= n2 then 1 else 0)
  | arithLeq    (Rubytype.INT  n1, Rubytype.REAL n2) = intValue  (if real n1 <= n2 then 1 else 0)
  | arithLeq    (Rubytype.REAL n1, Rubytype.INT  n2) = intValue  (if n1 <= real n2 then 1 else 0)
  | arithLeq    (Rubytype.REAL n1, Rubytype.REAL n2) = intValue  (if n1 <= n2 then 1 else 0) 
 
fun arithEq     (Rubytype.INT  n1, Rubytype.INT  n2) = intValue  (if n1 = n2 then 1 else 0)
  | arithEq     (Rubytype.INT  n1, Rubytype.REAL n2) = intValue  (if Real.==(real n1,n2) then 1 else 0)
  | arithEq     (Rubytype.REAL n1, Rubytype.INT  n2) = intValue  (if Real.==(n1,real n2) then 1 else 0)
  | arithEq     (Rubytype.REAL n1, Rubytype.REAL n2) = intValue  (if Real.==(n1,n2) then 1 else 0) 
 
fun arithNot    n = if n=1 then intValue 0 else if n=0 then intValue 1
                    else Errors.def_error "Not n -- n needs to be 0 or 1"

(*************************************************************************)
(**************************************************************************)

end (* of structure ArithPrims *);
(* open ArithPrims *)
