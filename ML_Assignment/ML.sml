Control.Print.printDepth := 100;
Control.Print.printLength := 100;
(*1*)
fun f g k x = [k [g x]]
(*2*)
fun bar x [] = []
|  bar x (g::gs) = (x * g)::(bar x gs)
(*3*)
fun part x [] = ([],[])
|   part x (g::gs) = let val (a,b) = part x gs
                      in if g < x then (g::a,b) else (a,g::b) end
                      
(*4*)                      
fun partSort [] = []
|   partSort [x] = [x]
|   partSort (x::xs) = let val (a,b) = part x xs in
                               (partSort a) @ [x] @ (partSort b) end
                               
(*5*)                               
fun pSort (op <) [] = []
|   pSort (op <) [x] = [x]
|   pSort (op <) (x::xs) = let fun pPart g [] = ([],[])
                               |   pPart g (y::ys) = let val (a,b) = pPart g ys in
                                                       if y < g then (y::a,b) else (a,y::b) end
                           in let val (c,d) = pPart x xs in
                                               (pSort (op <) c) @ [x] @ (pSort (op <) d) end end
                                               
                                               
(*6*)
exception reduce_Error
fun reduce g [] = raise reduce_Error
|   reduce g [x] = x
|   reduce g (x::xs) = g x (reduce g xs)


(*7*)
datatype 'a tree = leaf of 'a | node of 'a tree list

(*8*)
fun fringe (leaf x) = [x]
|   fringe (node x) = reduce (fn a => fn b => a@b) (map fringe x)

(*9*)
fun sortTree (op <) (leaf x) = leaf (pSort (op <) x)
|   sortTree (op <) (node x) = node (map (sortTree (op <)) x)

(*10*)
fun powerSet [] = [[]]
|   powerSet (x::xs) = let fun f a b = a::b
                       in
                        (map (f x) (powerSet xs)) @ (powerSet xs)
                       end