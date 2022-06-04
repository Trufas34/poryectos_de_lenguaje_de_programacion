prim :: Int->[Int]->[((Int,Int),Int)]->[((Int,Int),Int)]->[((Int,Int),Int)] 
if elem limite entrada then arm else if porPasar2 && porPasar1 then añadirCoincidencia else buscarSiguiente
prim limite entrada salida arm (arista:grafo) | elem limite entrada = arm
                                            |


                                           where porPasar1 =((not((snd (fst arista))==fst (fst arista)))&&(notElem (snd (fst arista)) salida)&&(elem (fst (fst arista)) entrada))      
                                                 añadirCoincidencia = arm++[arista]++prim limite ([snd (fst arista)]++entrada) ([fst (fst arista)]++salida) arm (ordenarLista grafo)
                                                 buscarSiguiente  = prim limite (entrada) salida arm (grafo++[arista])

                                                 