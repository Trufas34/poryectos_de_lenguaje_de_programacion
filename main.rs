/*
Nombre: Jeremy Gonzalez C:I 27.403.276
EBNF
<oracion>::=<Sujeto><Predicado>.
<Sujeto>::=<nombre>|<arcticulo><sustantivo>
<Predicado>::=<verbo>[<Sujeto>[<Adjetivo>]|<Adjetivo>|<Preposicion><Sujeto>|<Adverbio>[<Preposicion><Sujeto>|<Sustantivo>|<Adjetivo>]]
*/
use std::fs::File;
use std::io::{BufRead, BufReader, Write};
use std::collections::HashMap;


fn main() {
   lectura_de_archivo();
}
//Se realiza la lectura y escritura en el archivo de entrada y de salidad, 
//por medio de un BUfreader y Write respectivamente
fn lectura_de_archivo(){
    let archivo = abrir_archivo();
    let mut salida = crear_archivo_de_salida();
    let  reader = BufReader::new(archivo);  

    //Recorremos el archivo de imput
    for (i, line) in reader.lines().enumerate(){
        let mut line : String = line.unwrap();
                  
        
       if line.ends_with('.'){
        //line = line.trim().to_string();
   
        line = line.to_lowercase();
        line = line.trim().to_string();
        line.pop();
                    
        let _aux = separar_oracion(&line);
        let aux1 = detector_de_tipo_palabra(&_aux);

         if analisador_lexico(&_aux){
          if analisador_sintactico(&aux1){
             writeln!(&mut salida,"oracion {} ok", i).unwrap();
         }else{
             writeln!(&mut salida,"oracion {} error sintactico", i).unwrap();
          }
     }else{
        writeln!(&mut salida,"oracion {} error sintactico", i).unwrap();
     }
       }else{
        writeln!(&mut salida,"oracion {} error sintactico", i).unwrap();
       }
          
        

}
}
//Abrimos el archivo input
fn abrir_archivo()->File{
    let name_file : &str = "src/oraciones.txt";
    let entrada : File = File::open(name_file).unwrap();
    entrada
}
//creamos el archivo output
fn crear_archivo_de_salida()->File{
    let file = File::create("src/salida.txt").unwrap();
    file
}
//usamos el metodo split para separa una cadena y distribuirla en un vector
fn separar_oracion(oracion: &str)-> Vec<&str>{
    let separada: Vec<&str> =oracion.split(" ").collect();
    separada
}


//Iteramos con cada palabra comprobando si pertece al dicionario
fn analisador_lexico(_oracion : &Vec<&str>)-> bool
{ 
   
   
    //comprovamos que las palabras esten dentro del diccionario
    for palabra in _oracion {
       
        if !palabra_correcta(palabra){
            return false;
        }
    }
   return true;
        
    

}
//verificamos iterando con las claves del diccionario para verificar la valides de la palabra
fn palabra_correcta(p: &str)->bool{
     //creamos un diconario
     let diccionario = crear_diccionario();

    for (_key, valor) in diccionario.iter(){
        if valor.contains(&p){
            return true;
            
        }
    }
    return false;
}
/*comparamos cada palabra con las pertenecientes, si la palabra existe entonces se guardara 
en un vector la clave a la que pertenece, para obtener el orden de los tipos de las palabras
*/
fn detector_de_tipo_palabra<'a>(_oracion : &'a Vec<&str>)-> Vec<&'a str>
{ 
    let mut conteo= Vec::<&str>::new();
    //creamos un diconario
    let diccionario = crear_diccionario();
    //comprovamos que las palabras esten dentro del diccionario
    for palabra in _oracion {
       
        for (_key, valor) in diccionario.iter(){
            if valor.contains(palabra){
                conteo.push(_key);                
            }
        }
    }
    
        return conteo; 
    }

/*Verificamos si el vector creado en tipo de palabra se puede generar con la gramatica
  esto lo hacemos iterando con un hashmap donde estan todos los posibles vectores
*/
fn analisador_sintactico(orden: &Vec<&str>)->bool 
{
      let reglas = crear_gramatica();
       for _i in orden{
       
        for (_key, valor) in reglas.iter(){
            if !(valor != orden){
                return true;
            }
        }
    }
return false;
}
   //posibles vectores  oracion
fn crear_gramatica<'a>()-> HashMap<&'a str, Vec<&'a str>>{
    let gramatica= HashMap::from([
        ("tipo1",vec!["nombre","verbo"]),
        ("tipo2",vec!["articulo","sustantivo","verbo"]),
        ("tipo3",vec!["nombre","verbo","nombre"]),
        ("tipo4",vec!["articulo","sustantivo","verbo","nombre"]),
        ("tipo5",vec!["nombre","verbo","articulo","sustantivo"]),
        ("tipo6",vec!["articulo","sustantivo","verbo","articulo","sustantivo"]),
        ("tipo7",vec!["nombre","verbo","nombre","adjetivo"]),
        ("tipo8",vec!["articulo","sustantivo","verbo","nombre","adjetivo"]),
        ("tipo9",vec!["nombre","verbo","articulo","sustantivo","adjetivo"]),
        ("tipo10",vec!["articulo","sustantivo","verbo","articulo","sustantivo","adjetivo"]),
        ("tipo11",vec!["nombre","verbo","adjetivo"]),
        ("tipo12",vec!["articulo","sustantivo","verbo","adjetivo"]),
        ("tipo13",vec!["nombre","verbo","preposicion","nombre"]),
        ("tipo14",vec!["articulo","sustantivo","verbo","preposicion","nombre"]),
        ("tipo15",vec!["nombre","verbo","preposicion","articulo","sustantivo"]),
        ("tipo16",vec!["articulo","sustantivo","verbo","preposicion","articulo","sustantivo"]),
        ("tipo17",vec!["nombre","verbo","advervio","preposicion","nombre"]),
        ("tipo18",vec!["articulo","sustantivo","verbo","adverbio","preposicion","nombre"]),
        ("tipo19",vec!["nombre","verbo","adverbio","preposicion","articulo","sustantivo"]),
        ("tipo20",vec!["articulo","sustantivo","verbo","adverbio","preposicion","articulo","sustantivo"]),
        ("tipo21",vec!["nombre","verbo","adverbio","sustantivo"]),
        ("tipo22",vec!["articulo","sustantivo","verbo","adverbio","sustantivo"]),
        ("tipo23",vec!["nombre","verbo","adverbio","adjetivo"]),
        ("tipo24",vec!["articulo","sustantivo","verbo","adverbio","adjetivo"])
        
       ]);

       gramatica
}
//diccionario de terminos validos
fn crear_diccionario<'a>()->HashMap<&'a str, Vec<&'a str>>{
    let lexico = HashMap::from([
        ("nombre", vec!["rosa", "maria", "carlota", "lucia", "juan", "diego", "luis", "jesus"]),
        ("articulo", vec!["la", "las", "el", "los", "un", "una", "unos", "unas"]),
        ("sustantivo", vec!["fruta","frutas", "perro", "perra", "gato", "gata", "niño", "niña", "arbol","perros", 
             "perras", "gatos", "gatas", "pelota", "pelotas", "niños", "niñas", "arboles"]),
        ("verbo", vec!["juega", "juegan", "come", "comen", "quiere", "quieren", "es", "son", 
            "corre", "corren", "llora", "lloran"]),
        ("preposicion", vec!["a", "con", "como", "por"]),
        ("adverbio", vec!["poco", "poca", "mucho", "mucha", "muy"]),
        ("adjetivo", vec!["rapido", "rapidos", "grande", "grandes", "verde", "verdes", "roja", 
            "rojas", "pequeño", "pequeños"])
    ]);

    lexico
}


