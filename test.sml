(* test harness ... *)

fun r f = (MonoWireCount := 0;
           PolyWireCount := 0;
           CurrentPrim := "";
           SourceFiles := [];
           IncludeFiles := [];
           FunStack := [];
           TextPosition := (0,0);
           Testing := true;
           rc (["rc",f],[]);
           Testing := false
          )

fun ra f = (MonoWireCount := 0;
           PolyWireCount := 0;
           CurrentPrim := "";
           SourceFiles := [];
           IncludeFiles := [];
           FunStack := [];
           Testing := true;
           rc (["rc","-a",f],[]);
           Testing := false
          )

fun rb f = (MonoWireCount := 0;
           PolyWireCount := 0;
           CurrentPrim := "";
           SourceFiles := [];
           IncludeFiles := [];
           FunStack := [];
           Testing := true;
           rc (["rc","-b",f],[]);
           Testing := false
          )

fun rx f = (MonoWireCount := 0;
           PolyWireCount := 0;
           CurrentPrim := "";
           SourceFiles := [];
           IncludeFiles := [];
           FunStack := [];
           Testing := true;
           rc (["rc","-x",f],[]);
           Testing := false
          )

