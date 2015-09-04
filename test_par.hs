-- test of par algorithm in Haskell
-- taken from definition of par in compose.sml

-- SML version, for reference
--fun par ps = let  fun par' []      = ([], [], [])
--                    | par' (p::ps) = let  val (dom,  ran,  rel)  = p
--                                          val (doms, rans, rels) = par' ps
--                                     in   (dom::doms, ran::rans, rel@rels)
--                                     end
--                  val (ds, rs, relation) = par' ps
--             in   (list ds, list rs, relation)
--             end
--


-- Haskell version
--par ps =
--	let (ds, rs, relation) = par' ps
--	in (ds, rs, relation)


par' [] = ([], [], [])
par' (p:ps) = let (dom, ran, rel) = p in
			  let (doms, rans, rels) = par' ps in
				(dom:doms, ran:rans, rel++rels)

-- equiv of fst, snd for 3-elem tuples
fst3 (a, b, c) = a
snd3 (a, b, c) = b
thd3 (a, b, c) = c

-- equivalent using above defs
mypar l = (map fst3 l, map snd3 l, concat $ map thd3 l)

-- and another
mypar2 l = (\(a, b, c) -> (a, b, concat c)) $ unzip3 l
