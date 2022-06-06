--Nombre: Jeremy Gonzalez C.I:27.403.276
--El foramta del archivo de entrada debe ser 
--(A B C D E F G)
--[0 4 3 7 0 0 0]
--[4 0 0 1 0 4 0]
--[3 0 0 3 5 0 0]
--[7 1 3 0 2 2 7]
--[0 0 5 2 0 0 2]
--[0 4 0 2 0 0 4]
--[0 0 0 7 2 4 0]
--[A G]
--Funcion para crear lista de aristas de un nodo con el formato [[((Int,Int),Int)]]
listaArista ::[[Int]]->[[((Int,Int), Int)]]
listaArista xss = [[((n,x),(xss!!n)!!x)|x<-[0..length xss-1 ]]|n<-[0..length xss-1 ]]


--Ordena la lista de aristas, de forma que la cabeza de la lista sea la arista de menor peso 
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

--Devuelve la arista de menor peso de una lista de aristas
menorAristaValidad ::[((Int,Int), Int)]->((Int,Int),Int)
menorAristaValidad [a] = a
menorAristaValidad xs = if null xs then head xs else menorAristaValidad (menorArista xs)

--Le da el formato [((Int,Int),Int)] a la lista generada por ListaArista 
aristas ::[[((Int,Int), Int)]]->[((Int,Int),Int)]
aristas [a] = a
aristas  (xs:xss) = if null xss then [] else xs++aristas xss

--Saca la arista de la lista de arista generada en Aristas
sacarNodo::((Int,Int),Int)->[((Int,Int),Int)]->[((Int,Int),Int)]
sacarNodo a [] = []
sacarNodo x [a] = if x==a then [] else [a]
sacarNodo c (xs:xss) = if null xss then [xs] else if xs == c then []++sacarNodo c xss else [xs]++sacarNodo c xss 

--Ordena la lista de mayor a menor por el peso de estas y elimina las aristas de peso 0 
ordenarLista::[((Int,Int),Int)]->[((Int,Int),Int)]
ordenarLista [a] =[a]
ordenarLista [] = []
ordenarLista xss =if snd (head xss) == 0 then ordenarLista (drop 1 xss) else if  null xss then xss else  ordenada
                  where ordenada =   [menorAristaValidad xss]++ordenarLista (sacarNodo (menorAristaValidad xss) xss)
                              
--Funcion que genera el ARM del grafo representado en el formato de aristas [((Int,Int),Int)]
prim :: Int->[Int]->[Int]->[((Int,Int),Int)]->[((Int,Int),Int)]->[((Int,Int),Int)] 

prim limite entrada salida arm (arista:grafo) | elem limite entrada = arm
                                              | porPasar1 = arm++[arista]++añadirCoincidencia
                                              | not (porPasar1) = buscarSiguiente
                                                 where porPasar1 =((not((snd (fst arista))==fst (fst arista)))&&(notElem (snd (fst arista)) salida)&&(elem (fst (fst arista)) entrada))      
                                                       añadirCoincidencia = (prim limite ([snd (fst arista)]++entrada) ([fst (fst arista)]++salida) arm (ordenarLista grafo))
                                                       buscarSiguiente  = prim limite (entrada) salida arm (grafo++[arista])

                                                 
--Funcion que revisa cuales son los camino que salen de un nodo, en una listas de aristas
caminosDeUnNodo::Int->[((Int,Int),Int)]->[((Int,Int),Int)]
caminosDeUnNodo y [] = []
caminosDeUnNodo y [a] = if y == fst (fst a)  then [a] else []
caminosDeUnNodo y (xs:xss) =if null xss then [] else if y == snd (fst xs) || y == fst (fst xs) then [xs]++caminosDeUnNodo y xss else []++caminosDeUnNodo y xss 

--Funcion que usando CaminosDeUnNodo dice si existen camino asia un nodo o no 
existenCaminos:: [((Int,Int),Int)]->Bool
existenCaminos xs =  length xs > 0 

--Funcion que drope una lista sin el que resive 
dropeo :: Int->[Int]->[Int]
dropeo y (x:xs) = if null xs then [] else if x == y then dropeo y xs else [x]++(dropeo y xs)

--Funcion que genera el camino mas corto desde un nodo inicio a un nodo fin
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

--Funcion que le da formato a Dijsktra
caminoMasCorto::[((Int,Int),Int)]->Int->Int->[Int]->Int->[Int]
caminoMasCorto arm ini fin visitados a = [ini]++(dropeo ini(dijsktra arm ini fin visitados a))++[fin]

--funcion que elimina los elementos head y last de una lista
eliminarbordes :: [Char]->[Char]
eliminarbordes xs = init (drop 1 xs)

--funcion que separa en cadenas de nombres la primera cadena del archivo
nombreNodos ::[Char]->[[Char]]
nombreNodos xs = words xs 

--funcion que devuelve a una tupla entre los nombres de los nodos y un numero que lo identifica
asociar :: [[Char]]->[([Char],Int)]
asociar xs = zip xs [0..length xs]

--Funcion que busca una similitud entre una palabra y la lista de tupas de nombre y indices, para devolver el equivalente en nombre del indice que se le da 
volverNombre::Int->[([Char],Int)]->[Char]
volverNombre y [a] | y == snd a = fst a 
volverNombre y (x:xs) = if null xs then [] else if y == snd x then fst x else volverNombre y xs 

--vuelve una lista de enteros una lista de [Char] correspiente a sus indices en la tupla asociar
volvernombre ::[Int]->[([Char],Int)]->[[Char]]
volvernombre (y:ys) xs = if null ys then [volverNombre y xs] else [volverNombre y xs]++(volvernombre  ys xs)

--Funcion que hace lo mismo que volverNombre solo que de nombre a numero
volverNumero :: [Char]->[([Char],Int)]->Int
volverNumero y [a] | y == fst a = snd a
volverNumero y (x:xs) = if null xs then 0 else if y == fst x then snd x else (volverNumero y xs)

--Funcion que le da formato de cadena a las Aristas
convertirTupla ::[((Int,Int),Int)]->[([Char],Int)]->[(([Char],[Char]),Int)]
convertirTupla (y:ys) xs = if null ys then [((nombrePrimero, nombreSegundo),peso)] else [((nombrePrimero, nombreSegundo),peso)]++(convertirTupla ys xs) 
                          where primero = fst (fst y)
                                segundo = snd (fst y)
                                peso = snd y
                                nombrePrimero =  volverNombre primero xs
                                nombreSegundo = volverNombre segundo xs

--se lee un archivo y se le da el formato de entrada necesario para trabajar 
formato :: IO ([[Char]],[[Int]],[[Char]])
formato = do 
      entrada<- readFile "entrada.txt" 
      let contenido = lines entrada
      let nodos = head contenido
      let puntos = last contenido
      let matriz = drop 1 (init contenido)
      let nodosFormato = (nombreNodos (eliminarbordes nodos))
      let puntosFormato = nombreNodos (eliminarbordes puntos)
      let matrizAntePreformato = map eliminarbordes matriz
      let matrizPreFormato = (map words matrizAntePreformato) 
      let matrizFormato = map (map read) matrizPreFormato
    
      return (nodosFormato,matrizFormato,puntosFormato)

--Se ejecuta el programa
main :: IO ()
main = do
(nodos,matriz,limites) <- formato
let tuplaAsociada = asociar nodos
let nodosNumereicos = asociar nodos
let primero = volverNumero (head limites) tuplaAsociada
let fin = volverNumero (last limites) tuplaAsociada
let matrizFormatos = ordenarLista (aristas (listaArista matriz))
let limitePrim = (length matrizFormatos) - (length nodos)
let arbol = (prim fin [primero] [primero] [] matrizFormatos) 
let rme =volvernombre (caminoMasCorto arbol primero fin [primero] 0) tuplaAsociada
let arbolFormato = convertirTupla arbol tuplaAsociada


print "El arbol de expansion minimo"
print arbolFormato
print "El camino mas eficiente"
print rme
return ()