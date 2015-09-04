(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This is a small vhdl  library                                   ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure VHDL_Library (*:
   sig
        val lib :  string
  end*) = 
struct

val vhdl_not = 
            "\nLibrary IEEE;                             \n"  ^ 
	    "\nuse IEEE.STD_LOGIC_1164.all;              \n"  ^        
            "\nentity INV is                             \n"  ^              
            "\nport (I: in std_logic; O: out std_logic); \n"  ^             
            "\nend INV;                                 \n\n" ^      
            "\narchitecture Behave_INV of INV is          \n" ^
            "\nbegin                                      \n" ^       
            "\nO <= ( not I);                             \n" ^ 
            "\nend Behave_INV;                          \n\n" ^   
            "\nconfiguration Configure_INV of INV is      \n" ^        
            "\nfor  Behave_INV                            \n" ^     
            "\nend for;                                   \n" ^   
            "\nend Configure_INV;                         \n"      

val vhdl_and = 
            "\nLibrary IEEE;                              \n" ^ 
            "\nuse IEEE.STD_LOGIC_1164.all;               \n" ^    
            "\nentity AND2 is                             \n" ^
            "\nport (I0,I1: in std_logic; O: out std_logic);\n" ^  
            "\nend AND2;                                   \n\n" ^
            "\narchitecture Behave_AND2 of AND2 is        \n" ^
            "\nbegin                                      \n" ^   
            "\nO <= ( I0 and I1 );                        \n" ^     
            "\nend Behave_AND2;                           \n\n" ^ 
            "\nconfiguration Configure_AND2 of AND2 is     \n" ^    
            "\nfor  Behave_AND2                            \n" ^   
            "\nend for;                                    \n" ^    
            "\nend Configure_AND2;                         \n" 
       
val vhdl_or =          
            "\nLibrary IEEE;                               \n" ^           
            "\nuse IEEE.STD_LOGIC_1164.all;                \n" ^     
            "\nentity OR2 is                               \n" ^     
            "\nport (I0,I1: in std_logic; O: out std_logic);\n" ^             
            "\nend OR2;                                   \n\n" ^        
            "\narchitecture Behave_OR2 of OR2 is           \n" ^    
            "\nbegin                                       \n" ^
            "\nO <= ( I0 or I1 );                          \n" ^  
            "\nend Behave_OR2;                            \n\n" ^    
            "\nconfiguration Configure_OR2 of OR2 is        \n" ^      
            "\nfor  Behave_OR2                              \n" ^    
            "\nend for;                                     \n" ^     
            "\nend Configure_OR2;                           \n"     

val vhdl_xor = 
            "\nLibrary IEEE;                                \n" ^   
            "\nuse IEEE.STD_LOGIC_1164.all;                 \n" ^  
            "\nentity XOR2 is                               \n" ^
            "\nport (I0,I1: in std_logic; O: out std_logic);\n" ^             
            "\nend XOR2;                                    \n\n" ^      
            "\narchitecture Behave_XOR2 of XOR2 is          \n" ^       
            "\nbegin                                        \n" ^  
            "\nO <= ( I0 xor I1 );                          \n" ^  
            "\nend Behave_XOR2;                             \n\n" ^    
            "\nconfiguration Configure_XOR2 of XOR2 is      \n" ^      
            "\nfor  Behave_XOR2                             \n" ^   
            "\nend for;                                     \n" ^    
            "\nend Configure_XOR2;                          \n"  

val vhdl_conf_xor = 
            "     for all : XOR2 use                \n"^
            "     configuration work.Configure_XOR2;\n"^
            "     end for;                          \n"
val vhdl_conf_and = 
            "     for all : AND2 use                \n" ^
            "     configuration work.Configure_AND2;\n" ^
            "     end for;                          \n"
val vhdl_conf_or  =
            "     for all : OR2 use                 \n" ^
            "     configuration work.Configure_OR2; \n" ^
            "     end for;                          \n" 
val vhdl_conf_not =
            "     for all : INV use                 \n" ^
            "     configuration work.Configure_INV  \n" ^
            "     end for;                          \n"
                 

val  lib = [("and",vhdl_and),
            ("or", vhdl_or),
            ("not",vhdl_not),
            ("xor",vhdl_not)]

end (* of structure VHDL_Library *);
(* open VHDL_Library *)
