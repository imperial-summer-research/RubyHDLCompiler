(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Functions for matching values in an "io" structure to specific  ***)
(***    wires and constants:                                            ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure IoAlias :
  sig
        val ioAlias : (string * Circuittype.expr * Rubytype.io) -> (Circuittype.exprtype*Rubytype.io) list
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    ioAlias is used in device-specific output formatting, for       ***)
(***    matching domain and range wires in the circuit to those given   ***)
(***    in the data declarations:                                       ***)
(***                                                                    ***)
(**************************************************************************)


fun ioAlias (i, e, Rubytype.PROD []) = []
  | ioAlias (i, e, io)      = ioAliases i (!e, io)

and ioAliases i (w, io)
    = (case (w, io) of
	     (* tjt97 - reals again 
         (CON d1,  CONST d2) => if    d1=d2
                                then  []
                                else  Errors.simple_error ("cannot connect " ^
                                                    Show.showConst d1 ^ " to " ^
                                                    Show.showConst d2 ^ " in &" ^
                                                    i ^ "_io")
													*)
         (Circuittype.CON (Rubytype.BOOL d1),  Rubytype.CONST (Rubytype.BOOL d2)) => if    d1=d2
                                then  []
                                else  Errors.simple_error ("cannot connect " ^
                                                    Show.showConst (Rubytype.BOOL d1) ^ " to " ^
                                                    Show.showConst (Rubytype.BOOL d2) ^ " in &" ^
                                                    i ^ "_io")
       | (Circuittype.CON (Rubytype.INT d1),  Rubytype.CONST (Rubytype.INT d2)) => if    d1=d2
                                then  []
                                else  Errors.simple_error ("cannot connect " ^
                                                    Show.showConst (Rubytype.INT d1) ^ " to " ^
                                                    Show.showConst (Rubytype.INT d2) ^ " in &" ^
                                                    i ^ "_io")
       | (Circuittype.CON (Rubytype.REAL d1),  Rubytype.CONST (Rubytype.REAL d2)) => if    Real.==(d1,d2)
                                then  []
                                else  Errors.simple_error ("cannot connect " ^
                                                    Show.showConst (Rubytype.REAL d1) ^ " to " ^
                                                    Show.showConst (Rubytype.REAL d2) ^ " in &" ^
                                                    i ^ "_io")
       | (Circuittype.CON (Rubytype.SYM d1),  Rubytype.CONST (Rubytype.SYM d2)) => if    d1=d2
                                then  []
                                else  Errors.simple_error ("cannot connect " ^
                                                    Show.showConst (Rubytype.SYM d1) ^ " to " ^
                                                    Show.showConst (Rubytype.SYM d2) ^ " in &" ^
                                                    i ^ "_io")
       | (Circuittype.CON _,   Rubytype.VAR _)    => [(w, io)]
       | (Circuittype.WIRE _,  Rubytype.VAR _)    => [(w, io)]
       | (Circuittype.WIRE _,  Rubytype.CONST _)  => [(w, io)]
       | (Circuittype.LIST es, Rubytype.PROD ios) => if    length es = length ios
                                then  Utilities.flatmap (ioAliases i)
                                              (Utilities.zip (map (!) es, ios))
                                else  Errors.simple_error ("cannot match " ^ i ^
                                                    " to &" ^ i ^ "_io")
       | (Circuittype.EXPR x,  _)        => ioAliases i (!x, io)
       | (_,       _)        => Errors.simple_error ("cannot match " ^ i ^
                                              " to &" ^ i ^ "_io")
      )

(**************************************************************************)
(**************************************************************************)

end (* of structure IoAlias *);
(* open IoAlias *)
