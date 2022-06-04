--Funcion para crear lista de aristas de un nodo
listaArista ::[[Int]]->[[((Int,Int), Int)]]

listaArista xss = [[((n,x),(xss!!n)!!x)|x<-[0..length xss-1 ]]|n<-[0..length xss-1 ]]
--Funciona para crear un ARM de un  Grafo

menorArista::[((Int,Int), Int)]->[((Int,Int),Int)]
menorArista [] = []
menorArista xs | (menorPri ||  ultimoCero) && not primeroCero = init xs
               | (menorLast ||  primeroCero) && not ultimoCero = drop 1 xs
               | ultimoCero && primeroCero = drop 1 (init xs)
               where primeroCero =
                      snd (head xs) == 0 
                     ultimoCero = snd (last xs) == 0 
                     menorPri = snd (head xs) <= snd (last xs) 
                     menorLast = snd (head xs) > snd (last xs)

menorAristaValidad ::[((Int,Int), Int)]->((Int,Int),Int)
menorAristaValidad [a] = a
menorAristaValidad xs = if null xs then head xs else menorAristaValidad (menorArista xs)

aristas ::[[((Int,Int), Int)]]->[((Int,Int),Int)]
aristas [a] = a
aristas  (xs:xss) = if null xss then [] else xs++aristas xss

vistarNodo :: Int->Int->[Int]->[Int]
vistarNodo inicio fin visitados|(not (elem inicio visitados))&&(not (elem fin visitados)) = visitados++[inicio]++[fin]
                               |not (elem inicio visitados) = visitados++[inicio]
                               |not (elem fin visitados) = [fin]++visitados

sacarNodo::((Int,Int),Int)->[((Int,Int),Int)]->[((Int,Int),Int)]
sacarNodo a [] = []
sacarNodo x [a] = if x==a then [] else [a]
sacarNodo c (xs:xss) = if null xss then [xs] else if xs == c then []++sacarNodo c xss else [xs]++sacarNodo c xss 

ordenarLista::[((Int,Int),Int)]->[((Int,Int),Int)]
ordenarLista [a] =[a]
ordenarLista [] = []
ordenarLista xss =if snd (head xss) == 0 then ordenarLista (drop 1 xss) else if  null xss then xss else  ordenada
                  where ordenada =   [menorAristaValidad xss]++ordenarLista (sacarNodo (menorAristaValidad xss) xss)
                              

prim :: Int->[Int]->[Int]->[((Int,Int),Int)]->[((Int,Int),Int)]->[((Int,Int),Int)] 

prim limite entrada salida arm (arista:grafo) | elem limite entrada = arm
                                              | porPasar1 = arm++[arista]++añadirCoincidencia
                                              | not (porPasar1) = buscarSiguiente
                                                 where porPasar1 =((not((snd (fst arista))==fst (fst arista)))&&(notElem (snd (fst arista)) salida)&&(elem (fst (fst arista)) entrada))      
                                                       añadirCoincidencia = (prim limite ([snd (fst arista)]++entrada) ([fst (fst arista)]++salida) arm (ordenarLista grafo))
                                                       buscarSiguiente  = prim limite (entrada) salida arm (grafo++[arista])

                                                 

caminosDeUnNodo::Int->[((Int,Int),Int)]->[((Int,Int),Int)]
caminosDeUnNodo y [] = []
caminosDeUnNodo y [a] = if y == fst (fst a) || y == snd (fst a) then [a] else []
caminosDeUnNodo y (xs:xss) =if null xss then [] else if y == snd (fst xs) || y == fst (fst xs) then [xs]++caminosDeUnNodo y xss else []++caminosDeUnNodo y xss 

existenCaminos:: [((Int,Int),Int)]->Bool
existenCaminos xs =  length xs > 0 

concatenar:: Int-> [Int] -> [Int]
concatenar x xs = xs++[x]

dropeo :: Int->[Int]->[Int]
dropeo y (x:xs) = if null xs then [] else if x == y then dropeo y xs else [x]++(dropeo y xs)
dijsktra:: [((Int,Int),Int)]->Int->Int->[Int]->Int->[Int]
dijsktra arm ini fin visitados  x = if  fin == ini then if x == 0 then caminoTipo1 else caminoTipo2 else if not seDevuelve then vueltaAtras else if entraPrimero then masPrimero else proximiPaso
                                            where arista = fst (head arm) 
                                                  primero = fst arista
                                                  segundo = snd arista
                                                  vuetaAtras = last (init visitados)
                                                  seDevuelve = existenCaminos (caminosDeUnNodo ini arm)
                                                  entraPrimero = (ini == primero)&&(not (elem segundo visitados))
                                                  caminoTipo1 =   (visitados++[fin])
                                                  caminoTipo2 = (drop (x-1) visitados)++[fin]
                                                  vueltaAtras = dijsktra (ordenarLista arm) (head (drop 1 visitados)) fin ((drop 1 visitados)++[head visitados]) (x+1) 
                                                  masPrimero = dijsktra (drop 1 arm) segundo fin (visitados++[primero]) x 
                                                  proximiPaso = dijsktra ((drop 1 arm)++[head arm]) ini fin visitados x  

caminoMasCorto::[((Int,Int),Int)]->Int->Int->[Int]->Int->[Int]
caminoMasCorto arm ini fin visitados a = [ini]++(dropeo ini(dijsktra arm ini fin visitados a))++[fin]