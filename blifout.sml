(************************************************************************)
(*				blifout.sml				*)
(************************************************************************)
(*		To output netlists in BLIF Format (for MIS2)		*)
(************************************************************************)

structure BlifOut : NETLIST_OUT =
struct

(************************************************************************)
fun signame s = " " ^ sym_of s
fun signames ss = strmap signame ss

(************************************************************************)
fun blif_block (Gate (gatetype, out, inputs)) =
    let val n = length inputs
        val fail = "FAILFAILFAILFAILFAILFAILFAILFAILFAILFAILFAILFAIL\n"
        fun or ss = implode ss ^ " 1\n"
        fun gate AND = implode (replicate "1" n) ^ " 1\n"
          | gate OR  = strmap or (interleave "1" (replicate "-" (n-1)))
          | gate XOR = if n = 2  then "01 1\n10 1\n"  else fail
          | gate INV = if n = 1  then "0 1\n"         else fail
     in outp (".names" ^ signames inputs ^ signame out ^ nl ^ gate gatetype)
    end
  | blif_block (b as (DType (q, d, ce, rd, clk))) =
    if sigs_eq ce Vcc
      then outp (".latch" ^ signame d ^ signame q ^
                 " re " ^ signame clk ^ " 0\n")
      else raise BAD_BLOCK b
  | blif_block b             =  raise BAD_BLOCK b
  ;

(************************************************************************)
(* Output blocklist in XNF form to filename *)
fun output_blocklist (pr as Program (params,_)) bl filename = 
let fun is_PadIn  (Pad (PadIn _))  = true
      | is_PadIn  _                = false
    fun is_PadOut (Pad (PadOut _)) = true
      | is_PadOut _                = false
    fun is_gate  (Gate _)  = true
      | is_gate   _        = false
    fun is_dtype (DType _) = true
      | is_dtype  _        = false
    fun unPad (Pad p) = p  | unPad _ = raise Match
    fun padsig_name p = " " ^ signame (padsig p)
    val in_pads  = map unPad (filter is_PadIn  bl)
    val out_pads = map unPad (filter is_PadOut bl)
    val (dtypes, rest) = partition is_dtype bl
    fun expand (DType (q, d, ce, rd, clk)) = 
          add_fd_c clk q (g_or [g_and [d,ce], g_and [q, g_inv ce]])
      | expand _ = raise Match
in
    (* Take out old dtypes - with clock enable *)
    BlockList := rest;
    (* Put in new dtypes with explicit clock enable circuitry *)
    app expand dtypes;
   
    output_to_file filename;
    outp ("# Handel Hardware Compiler Vn., C. Ian Page 1993\n");
    outp ("# Written to <" ^ filename ^ "> on " ^ unix_date ());
    outp (print_prog "\n#  " pr);  outp "\n";

    outp (".model " ^ filename ^ nl);
    outp (".inputs"  ^ strmap padsig_name  in_pads ^ nl);
    outp (".outputs" ^ strmap padsig_name out_pads ^ nl);
    outp ".clock GCLK\n";
    outp ".names GND\n";
    outp ".names VCC\n";
    outp "1\n";
   
    app blif_block (filter is_gate  (!BlockList));
    app blif_block (filter is_dtype  (!BlockList));
    outp (".end\n");
    output_to_std_out ();
    ()
end;

(************************************************************************)
end;	(* of struct BlifOut *)


(************************************************************************)
(* Testing LED protocol converter  :  for Jonathan Saul/BLIF *)
(*
val DW  = 7;
val R1  = MakeReg  ("R1",  DW);
val Ch1 = MakeChan ("Ch1", DW);
val LED_DATA = CPC_AsciiLED (Ch1, (["F2","E1","E2","F3","D2","C1","D1"], "C2"));
prog := Prog (
  [ LED_DATA ], 
  [ R1 ], 
  [ ("R1", R1) ],
  While (TRUE,
      Seq [ R1 := R1 + C 1,
            Ch1 !! R1
          ])
             
  );

cof();

output_blocklist ("tputer", "zz") (!prog) (!BlockList) "tputer";
*)
