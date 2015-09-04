(* tjt: this is adapted from printVHDL to print a      *)
(*simple structural verilog version of the circuit     *)



(******************************************************)
(*   These functions give a printable Veilog          *)
(*   representation of a pcircuit.                    *)
(******************************************************)

structure PrintVerilog :
  sig
        val printVerilog : Circuittype.pcircuit -> string
  end =
struct

(******************************************************)
(* number of nets to be printed in a line (for better *)
(*           appearance of the Verilog output)        *)
(*     and number of letters for each net name        *)     
(******************************************************)

val NUM_NETS = 4 

val NUM_LETTERS = 9 

(*******************************************************)
(* trimStringList will add a str to each element of    *)
(* string_list and put a CR to the (num+1) the elements*)
(*******************************************************)

fun trimStringList string_list num str= 
       let val num_list = ref 0
           fun trim_list []  = []
             | trim_list [x] = [x]
             | trim_list (x::xs) = 
                if (!num_list) = num 
                then let val dummy = num_list := 0
                     in 
              ((Utilities.ljustify (NUM_LETTERS-1) (x ^str^"\n"))
                               :: (trim_list xs)) 
                     end
                else let val dummy = 
                      num_list := (!num_list + 1)
                     in ((Utilities.ljustify NUM_LETTERS (x ^str)) 
                         :: (trim_list xs)) 
                     end
        in trim_list string_list
        end

val num_gates = ref 0;
val have_and  = ref false;
val have_or   = ref false;
val have_not  = ref false;
val have_xor  = ref false;
val have_GND  = ref false;
val have_VCC  = ref false;

(* tjt: flag: true iff we have used a dff *)
val have_dff = ref false;

val signal_table = ref ([] : string list)

val input_table  = ref ([] : string list)

val output_table = ref ([] : string list)



fun set_flag str = 
      if str = "and" then  have_and := true
      else if str = "or" then have_or := true
      else if str = "xor" then have_xor := true
	  (* tjt: delays *)
      else if str = "D ?" then have_dff := true
      else if str = "D T" then have_dff := true
      else if str = "D F" then have_dff := true
      else  have_not := true

(*************************************************)
(*       include component library               *)
(*************************************************)

fun include_lib _ = 
   let val part_and = if !have_and
                             then ""
                      else ""
       val part_or  = if !have_or
                             then ""
                      else ""
       val part_not = if !have_not
                             then ""
                      else ""
       val part_xor = if !have_xor
                             then ""
                      else ""
       val part_dff = if !have_dff
                             then "module ruby_dff" ^
							   "(output reg outp, input inp, input clk);\n" ^
							   "always @ (posedge clk)\n" ^
							   "   outp <= inp;\n" ^
							   "endmodule\n"
                      else ""
   in 
      part_and ^ part_or ^ part_not ^ part_xor 
	  (* tjt *)
	  ^ part_dff 
   end

fun include_lib_config _ = 
   let val part_and = if !have_and
                             then VHDL_Library.vhdl_conf_and 
                      else ""
       val part_or  = if !have_or
                             then VHDL_Library.vhdl_conf_or
                      else ""
       val part_not = if !have_not
                             then VHDL_Library.vhdl_conf_not
                      else ""
       val part_xor = if !have_xor
                             then VHDL_Library.vhdl_conf_xor
                      else ""
   in part_and ^ part_or ^ part_not ^ part_xor end        

(****************************************************)
(*produce component definitions in architecture part*)
(****************************************************)

fun include_comp_def _ = 
   let val part_and = 
         if !have_and
         then "\ncomponent AND2 \n" ^
              "    port(I0,I1: in std_logic; " ^ 
              "O: out std_logic);\n" ^
              "end component;\n"
         else ""
       val part_or  = 
         if !have_or
         then "\ncomponent OR2 \n" ^
             "    port(I0, I1: in std_logic; " ^
             "O: out std_logic);\n" ^
             "end component;\n"
         else ""
       val part_not = 
         if !have_not
         then "\ncomponent INV \n" ^
           "    port(I: in std_logic; " ^
           "O: std_logic);\n" ^
           "end component;\n"
         else ""
       val part_xor = 
         if !have_xor
         then "\ncomponent XOR2 \n" ^
          "    port(I0, I1: in std_logic; " ^
          "O: out std_logic);\n" ^
          "end component;\n"
       else ""
   in part_and ^ part_or ^ part_not 
      ^ part_xor  
   end     


(******************************************************)
(**  remember to evaluate input and output           **)
(**  ports befor evaluate other nets                 **)
(******************************************************)

fun put_nets net = let val table1 = !signal_table
                       val table2 = !input_table
                       val table3 = !output_table 
                   in if  (Utilities.elem net table1)     then () 
                      else if (Utilities.elem net table2) then ()
                      else if (Utilities.elem net table3) then ()
                      else signal_table := net :: table1
                   end

(******************************************************)
(** nets' names:                                     **)
(**    if a net is connected to a port, then don't   **)
(**    put it into the nets table                    **)
(******************************************************)

fun printExpr put_name e = (case !e of
          Circuittype.CON k       => [Show.showConst k]
 | Circuittype.WIRE (Circuittype.POLY,name,_) => let 
                            val net_name = 
                              "net_" ^ Int.toString name
                         in if put_name = "not_put" then
                                     [net_name]
                            else (put_nets net_name; [net_name])
                            end
 | Circuittype.WIRE (_,   name,_) => let 
                          val net_name = 
                             "net_" ^ Int.toString name
                         in if put_name = "not_put" then
                                    [net_name]
                           else (put_nets net_name; [net_name])
                            end
 | Circuittype.LIST es            => (Utilities.flatmap (printExpr put_name) es)
 | Circuittype.EXPR x             => printExpr put_name x
)

fun printGate (device, input, output) = 
     let  fun maplogic []      = (false,[])
            | maplogic (x::xs) = 
                  let val (b,nets)  = maplogic xs
                      val (b1,[net1]) = x
                  in (b orelse b1, net1 :: nets) 
                  end
          fun has_constant e = (case !e of
                Circuittype.CON k              => (true,printExpr "put" e)
              | Circuittype.WIRE (Circuittype.POLY,name,_) => (false, printExpr "put" e)
              | Circuittype.WIRE (_,   name,_) => (false, printExpr "put" e)
              | Circuittype.LIST es            => (maplogic (map has_constant es))
              | Circuittype.EXPR x             => (has_constant x) 
                 )
          val gate' = Show.showDevice device
          fun produce_name "and" = "AND2 "
            | produce_name "or"  = "OR2 "
            | produce_name "xor" = "XOR2 "
            | produce_name "not" = "INV "
			(* tjt: add flip-flip quasi-gates *)
            | produce_name "D ?" = "ruby_dff "
            | produce_name "D T" = "ruby_dff "
            | produce_name "D F" = "ruby_dff "
            | produce_name  x    = "NOT_DEFINED|" ^ x ^ "|"
          fun produce_component num nets_in nets_out =
              let fun process_VCC_GND gate  = 
                  if gate = "and" orelse gate = "or" 
                                  orelse gate = "xor"
                  then 
                     let val [x1,x2] = nets_in
                         val    [z]  = nets_out
						 (* tjt: specify formal param name in Verilog as *)
						 (* .formal(actual) *)
                         val input1 = 
                            if x1 = "F" then
                              (have_GND := true;
                                   ".I0(XGND)")
                            else if x1 = "T" then
                              (have_VCC := true;
                                   ".I0(XVCC)")
                            else   ".I0(" ^ x1 ^ ")"
                         val input2 = 
                            if x2 = "F" then
                               (have_GND := true;  
                                   ".I1(XGND)")
                            else if x2 = "T" then
                                (have_VCC := true;
                                   ".I1(XVCC)")
                            else   ".I1(" ^ x2 ^ ")"
                         val output = ".O(" ^ z ^ ")"
					(* tjt: different order from VHDL *)
                      in 
                                produce_name gate ^ 
					  	"U" ^ Int.toString (num: int) ^ 
                         " "   ^ 
                         "("    ^
                                ".I0(" ^ input1 ^ ")" ^ 
                         "," ^ 
                                ".I1(" ^ input2 ^ ")" ^ 
                         "," ^ 
                                ".O(" ^ output ^ ")" ^
                         ");\n"
                      end
					  (* tjt: previously, delays were not supported! *)
					  (* replace by call to our dff block *)
					  (* separate code for initial D values; could be
					  refactored *)
				  else if gate = "D ?" then
				  	let val [x_in] = nets_in
					    val [x_out] = nets_out
					in
				  	  produce_name gate ^ 
					  " U" ^ Int.toString (num: int) ^ 
					  "(" ^ x_out ^ ", " ^ x_in ^ ", clk);\n"
					end
				  else if gate = "D F" then
				  	"always@posedge(clk)\n"
				  else if gate = "D T" then
				  	"always@posedge(clk)\n"
                  else (* if gate =  "not" then *) 
                       let val [x1] = nets_in
                           val    [z]  = nets_out
                           val input1 = 
                               if x1 = "F" then
                                  (have_GND := true;
                                  ".I(XGND)")
                               else if x1 = "T" then
                                   (have_VCC := true;
                                   ".I(XVCC)")
                               else ".I(" ^ x1 ^ ")"
                           val output = ".O(" ^ z ^ ")"
                       in
                          produce_name gate ^ 
						  " U" ^ Int.toString (num: int) ^ 
						  "(" ^
                       input1 ^ "," ^ output ^");\n"
                       end
              in process_VCC_GND gate' end     
              val num  = !num_gates
              val dum1 = num_gates := num +1
              val dum2 = set_flag gate'
              val (b,nets_in)  = has_constant input
              val nets_out     = printExpr "put" output
          in produce_component num nets_in nets_out 
          end 


fun printRelation ds = Utilities.concatWith "" (map printGate ds)


fun printRelations [] = ""
  | printRelations rs = 
         let val part1 = 
                    Utilities.concatWith "" (map printRelation rs)
             val part2 = if !have_GND then "XGND <= '0';\n"
                         else ""
             val part3 = if !have_VCC then "XVCC <= '1';\n"
                         else ""
		(* tjt: remove "begin" and "end" *)
         in   "\n// begin\n" ^
                 part2 ^ part3 ^ part1 ^
              "// end\n"
         end

(*********************************************************************)
(*                     produce    ports                              *)
(*********************************************************************)

fun printInputs (dom, ran)
        = let  fun isInput e = (case !e of
                                  Circuittype.WIRE (Circuittype.IN  ,_,_)  => true
                                | Circuittype.WIRE (Circuittype.POLY,_,_)  => true
                                | _                => false
                               )
               val wires = Exprs.exprWires dom @ Exprs.exprWires ran
               val inputs =
			   (* tjt97 - can't get the remove duplicates to work
			   was:
                    orderedRemduprefs (Utilities.filter isInput wires)
					*)
                    (Utilities.filter isInput wires)
               val Inputs = Utilities.flatmap (printExpr "not_put") inputs 
               val dummy  = input_table := Inputs
			   (* tjt: separate inputs with "input" *)
               val inStr  = Utilities.concatWith "input " 
                 (trimStringList Inputs NUM_NETS ",")
			   (* tjt: prepend inputs with "input" *)
          in   "input " ^ inStr ^ " "                
          end

fun printOutputs (dom, ran)
        = let  fun isOutput e = (case !e of
                                  Circuittype.WIRE (Circuittype.OUT  ,_,_) => true
                                | _                => false
                               )
               val wires = Exprs.exprWires dom @ Exprs.exprWires ran
               val outputs =
			   (* tjt97 - do not remove duplicates again
			   was:
                   orderedRemduprefs (Utilities.filter isOutput wires)
				   *)
                   (Utilities.filter isOutput wires)
               val Outputs = 
                   Utilities.flatmap (printExpr "not_put") outputs
               val dummy   = output_table := Outputs
			   (* tjt: separate outputs with "output" *)
               val inStr = Utilities.concatWith "output " 
                  (trimStringList (Outputs) NUM_NETS ",")
			   (* tjt: prepend outputs with "output" *)
          in  "output " ^ inStr ^ ");\n"                
          end

(***************************************************************)
(**                       produce entity                      **)
(***************************************************************)

(* tjt: mod for Verilog *)
(* note: assume at least one input and one output
 *       hence: - prepend "input" to inputs
 *              - prepend "output" to outputs
 *              - separate inputs and outputs by ","
 *              - separate list of inputs and outputs by "input" or "output"
 *)
fun produce_entity (dom, ran) = "\nmodule current (\n" ^
                                printInputs (dom, ran) ^ ",\n" ^
                                printOutputs (dom, ran) ^
                                "\n"

(***************************************************************)
(**        produce signals: none port nets                    **)
(***************************************************************)

fun produce_signals _ = 
         let val dummy1  = 
                if !have_GND then         
                   signal_table := "XGND" :: (! signal_table)
                else ()
             val dummy2  = 
                if !have_VCC then         
                   signal_table := "XVCC" :: (! signal_table)
                else ()                          
             val signals = ! signal_table
             val str     = 
			 (* tjt: separate signals with "wire" *)
                 Utilities.concatWith "wire " 
                      (trimStringList signals NUM_NETS ",")
          in "\n// signals\n" ^ str ^ "\n"
          end


fun printVerilog (dom, ran, rels)
        = let 
            fun initial_var _ = 
               let val dum1 = num_gates := 0
                   val dum2 = have_and  := false
                   val dum3 = have_or   := false
                   val dum4 = have_not  := false
                   val dum5 = signal_table := []
                   val dum6 = input_table  := []
                   val dum7 = output_table := []
                   val dum8 = have_xor  := false
				   (* tjt *)
                   val dum9 = have_dff  := false
                in () end
            val head = 
			           (* tjt: was *)
			           (*"Library IEEE;\n" ^
                       "use IEEE.STD_LOGIC_1164.all;\n" ^
                       "use IEEE.STD_LOGIC_UNSIGNED.all;\n\n"*)
					   "// head\n"

            val dummy = initial_var ()
            val entity = produce_entity (dom, ran)
            val components = printRelations rels
            val signals    = produce_signals ()
            val comp_defs  = include_comp_def ()
            val archit     = 
			   (* tjt: was*)
               (*"\narchitecture Struct_current of current is\n"*)
			   "// archit\n"
            val lib        = include_lib ()
            val lib_config = include_lib_config ()
            val config     = 
              "configuration  Structure_current of current is\n" ^ 
              " for Struct_current\n" ^ lib_config ^
              " end for;          \n" ^
              "end Structure_current;\n"
            val head_work  = "// head_work\n\n"
              
          in head_work ^
		     "//lib\n" ^
             lib       ^
			 "//head\n" ^
             head      ^
			 "//entity\n" ^
             entity    ^ 
			 "//archit\n" ^
             archit    ^ 
			 "//signals\n" ^
             signals   ^ 
			 "//comp_defs\n" ^
             (* tjt: COMMENTED OUT comp_defs ^ *)
			 "//components\n" ^
             components ^
			 "//config\n" ^
             (*tjt: COMMENTED OUT config *)
			 "endmodule\n"
          end
end (* of structure PrintVerilog *);
(* open PrintVerilog *)
