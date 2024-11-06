import scala.annotation.tailrec

type Bit = 0 | 1
type TablaCodigos = List[(Char, List[Bit])]

abstract class ArbolHuffman {
  def peso: Int = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.peso + nodoDch.peso //Suma el peso de los dos nodos
    case HojaHuff(char, weight) => weight //Peso de la hoja

  def caracteres: List[Char] = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.caracteres ++ nodoDch.caracteres // Concatema ambos nodos 
    case HojaHuff(char, weight) => List(char) //Ya es el caso hoja

  def contieneCaracter(char: Char): Boolean = this match
    case HojaHuff(c, weight) => c == char // Comprueba si el c de la hoja es el que gemos introducido
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.contieneCaracter(char) || nodoDch.contieneCaracter(char) // Ve si alguno de los nodos lo contiene

  def decodificar(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(arbol: ArbolHuffman, bits: List[Bit], acc: String): String = (arbol, bits) match
      case (HojaHuff(char, weight), Nil) => acc + char //  hoja donde no hay más bits y añade el char al acc
      case (HojaHuff(char, weight), _) => decodificarAux(this, bits, acc + char)  //llega a una hoja pero quedan bits, agrega el char al acc y reinicia la decodificacion
      case (RamaHuff(nodoIzq, nodoDch), 0 :: restoBits) => decodificarAux(nodoIzq, restoBits, acc) //si el primer bit es 0 continua por la rama izquierda y llama a la funcion recursivamente
      case (RamaHuff(nodoIzq, nodoDch), 1 :: restoBits) => decodificarAux(nodoDch, restoBits, acc)  //si el primer bit es 1 continua por la rama derecha y llama a la funcion recursivamente
      case _ => acc  //detiene la decodificacion y devuelve el acc

    decodificarAux(this, bits, " ")

  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(listaChar: List[Char], acc: List[Bit]):  List[Bit] = listaChar match
      case Nil => acc //si la lista está vacía devuelve acc
      case char :: restoCadena => codificarAux(restoCadena, acc ++ codificarCaracteres(char, this, List.empty[Bit])) //si la lista contiene al menos un carácter, se concatena acc con la función

    codificarAux(cadenaAListaChars(cadena), List.empty[Bit])


  def codificarCaracteres(char: Char, arbol: ArbolHuffman, lista: List[Bit]): List[Bit] = arbol match
    case HojaHuff(caracter, weight) if caracter == char => lista //si nos encontramos en una hoja y contiene el caracter buscado devuelve la lista
    case RamaHuff(nodoIzq, nodoDch) =>
      if (nodoIzq.contieneCaracter(char)) codificarCaracteres(char, nodoIzq, lista :+ 0) //si el caracter se encuentra en el subarbol izquierdo, llama a la funcion y añade 0 a la lista
      else codificarCaracteres(char, nodoDch, lista :+ 1)  //si el caracter no se encuentra en el subarbol izquierdo, llama a la funcion en el nodo derecho y añade 1 a la lista
    case _ => throw new IllegalArgumentException("Caracter no encontrado") //si el caracter no se encuentra, lanza una excepcion
}

case class HojaHuff(char: Char, weight: Int) extends ArbolHuffman

case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman


def cadenaAListaChars(cadena : String) : List[Char] = cadena.toList 

def listaCharsACadena(listaCar : List[Char]) : String =
  @tailrec
  def listaCharsACadenaAux(listaCar : List[Char], acc : String) : String = listaCar match
    case Nil => acc 
    case head :: tail => listaCharsACadenaAux(listaCar, acc + head) 

  listaCharsACadenaAux(listaCar, " ") 

def listaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = //Recibe una lista de caracteres y devuelve una lista de tuplas

  @tailrec
  def listaCharsADistFrecAux(listaChar: List[Char], frecuencia: List[(Char, Int)]): List[(Char, Int)] = listaChar match //recibe la lista de caracteres que aun no se han procesado y una lista de tuplas que almacena las frecuencias acumuladas
    case Nil => frecuencia //si la lista está vacía, devuelve frecuencia
    case head :: tail => listaCharsADistFrecAux(tail, actualizarFrecuencia(head, frecuencia))  //si la lista tiene al menos un elemento, llama a la funcion con el primer elemento y la lista de frecuencias actuales

  //frecuencia de un caracter
  def actualizarFrecuencia(char: Char, frecuencia: List[(Char, Int)]): List[(Char, Int)] = frecuencia match 
    case Nil => List((char, 1)) //si la lista está vacía, este es el primer carácter. Crea una lista con la nueva tupla
    case (c, f) :: tail if c == char => (c, f + 1) :: tail    //si la frecuencia tiene al menos una tupla y el carácter actual ya está, incrementa la frecuencia
    case head :: tail => head :: actualizarFrecuencia(char, tail)   //si el caracter no coincide con el primero, llama recursivamente a la funcion en el resto de la lista

  listaCharsADistFrecAux(listaChar, List.empty[(Char, Int)])

def distribFrecAListaHojas(frecuencias: List[(Char, Int)]): List[HojaHuff] =
  val hojas = frecuencias.map { case (char, weight) => HojaHuff(char, weight) }   // Tuplas (char, Int) a hojas
  // Ordenar las hojas por peso.
  hojas.sortBy(_.peso)

def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuff = RamaHuff(izq, dch)

def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
  case head :: segundo :: tail => //  Extraemos dos primeros elementos
    //  Los combinamos en RamaHuff
    val nuevoArbol = RamaHuff(head, segundo)
    (nuevoArbol :: tail).sortBy(_.peso)
  case _ => nodos // Vacía devuelve la lista como esta

def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista match
  case _ :: Nil => true //si la lista solo tiene un elemento devuelve true
  case _ => false //en caso contrario, devuelve false

@tailrec
def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman])(esListaSingleton: List[ArbolHuffman] => Boolean)(listaHojas: List[ArbolHuffman]): List[ArbolHuffman] =
  if listaHojas == Nil then listaHojas 
  else if esListaSingleton(listaHojas) then listaHojas //si tiene un elemento devuelve la lista    
  else repetirHasta(combinar)(esListaSingleton)(combinar(listaHojas)) //si tiene más de un elemento, llama a la función hasta que tenga uno

def crearArbolHuffman(cadena: String): ArbolHuffman =
  val cadenaAChar = cadenaAListaChars(cadena) //convierte  cadena de texto en una cadena de carácteres
  val listaCharATuplas = listaCharsADistFrec(cadenaAChar) //convierte la lista de caracteres en una lista de tuplas
  val listaTuplasOrdenada = distribFrecAListaHojas(listaCharATuplas) //ordena la lista de tuplas por peso
  repetirHasta(combinar)(esListaSingleton)(listaTuplasOrdenada).head //construye el arbol con las tuplas ordenadas por peso
  
// Convertir arbol codificacion en tabla de codificacion
def deArbolATabla(arbol: ArbolHuffman): TablaCodigos = 
  def deArbolATablaAux(arbol: ArbolHuffman, bits: List[Bit]): TablaCodigos = arbol match
    case HojaHuff(char, weight) => List((char, bits)) // Si es hoja crea la lista de tuplas
    case RamaHuff(nodoIzq, nodoDch) => deArbolATablaAux(nodoIzq, bits :+ 0) ++ deArbolATablaAux(nodoDch, bits :+ 1)  // Lo con nodo izquierdo añadiendo bit=0 y con la dcha con bit=1
  
  deArbolATablaAux(arbol, List.empty[Bit])

def codificarTabla(tabla: TablaCodigos)(cadena: String): List[Bit] =
  
  @tailrec
  def codificarCaracter(tabla: TablaCodigos)(char: Char): List[Bit] = tabla match // Para un caracter
    case Nil => List.empty[Bit] // Devuelve lista vacia de bits
    case (c, bits) :: tail if c == char => bits // Si encuentra ese caracter que devuelva los bits de la tupla
    case _ :: tail => codificarCaracter(tail)(char) // Si no lo encuentra sigue buscando
 
  def codificarCadena(cadena: List[Char]): List[Bit] = cadena match // Para toda la cadena
    case Nil => List.empty[Bit] // Si la cadena está vacía devuelve lista vacía de bits  
    case char :: tail =>
      val bits = codificarCaracter(tabla)(char) // Obtiene los bits del carácter
      bits ++ codificarCadena(tail) // Concatena los bits y sigue con la parte de la cadena que queda

  codificarCadena(cadena.toList)
 
def decodificarTabla(tabla: TablaCodigos)(bitsDados: List[Bit]): String =
  @tailrec
  def decodificarCaracter(tabla: TablaCodigos)(bits: List[Bit]): (Char, List[Bit]) = tabla match
    case Nil => (' ', bits) // Si la tabla está vacía no hay correspondencia
    case (c, b) :: tail if bits.startsWith(b) => (c, bits.drop(b.length)) // Si los bits de la tabla coinciden  con los que piden devuelve el carácter y los bits restantes porque el .drop elimina los que coinciden y asi solo quedan los que no se han encontrado
    case _ :: tail => decodificarCaracter(tail)(bits) // Busca en el resto de la tabla
  
  @tailrec
  def decodificarCadena(tabla: TablaCodigos)(bits: List[Bit])(acc: String): String = bits match
    case Nil => acc // Si no quedan bits devuelve el acumulador
    case _ =>
      val (caracter, bitsRestantes) = decodificarCaracter(tabla)(bits)
      decodificarCadena(tabla)(bitsRestantes)(acc + caracter)

  decodificarCadena(tabla)(bitsDados)(" ")

object ArbolHuffman {
  def apply(cadena: String): ArbolHuffman =
    crearArbolHuffman(cadena)
}

object miPrograma extends App {

  //Crea el árbol
  val miArbol = RamaHuff(HojaHuff('S', 4), RamaHuff(HojaHuff('O', 3), RamaHuff(HojaHuff('E', 2), HojaHuff(' ', 2))))
  println(s"Mi Árbol: $miArbol")

  //Calcula el peso del árbol
  val weight = miArbol.peso
  println(s"Peso: $weight")

  //Cojo los caracteres del árbol
  val caracter = miArbol.caracteres
  println(s"Caracteres: $caracter")

  //Decodifica la lista
  val sec = miArbol.decodificar(List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0))
  println(s"Cadena decodificada: $sec")

  //Codifica la cadena
  val cod = miArbol.codificar("SOS ESE OSO")
  println(s"Cadena codificada: $cod")

  //Crea una lista de prueba
  val lista1 = List('a', 'b', 'c', 'd', 'e')
  val lista2 = List()
  val lista3 = List('a', 'b', 'a', 'c', 'a', 'a', 'd', 'e', 'b', 'c', 'a', 'b')
  val lista4 = List('a', 'b')
  println(s"Lista1: $lista1")
  println(s"Lista2: $lista2")
  println(s"Lista3: $lista3")
  println(s"Lista4: $lista4")

  //Compruebo la función listaCharsADistFrec
  val comprueboLista1 = listaCharsADistFrec(lista1)
  val comprueboLista2 = listaCharsADistFrec(lista2)
  val comprueboLista3 = listaCharsADistFrec(lista3)
  val comprueboLista4 = listaCharsADistFrec(lista4)
  println(s"Lista Char A Dist Frec Lista1: $comprueboLista1")
  println(s"Lista Char A Dist Frec Lista2: $comprueboLista2")
  println(s"Lista Char A Dist Frec Lista3: $comprueboLista3")
  println(s"Lista Char A Dist Frec Lista4: $comprueboLista4")

  //Compruebo la función distribFrecAListaHojas
  val probarDistrib1 = distribFrecAListaHojas(comprueboLista1)
  val probarDistrib2 = distribFrecAListaHojas(comprueboLista2)
  val probarDistrib3 = distribFrecAListaHojas(comprueboLista3)
  val probarDistrib4 = distribFrecAListaHojas(comprueboLista4)
  println(s"Probar funcion Distrib Lista1: $probarDistrib1")
  println(s"Probar funcion Distrib Lista2: $probarDistrib2")
  println(s"Probar funcion Distrib Lista3: $probarDistrib3")
  println(s"Probar funcion Distrib Lista4: $probarDistrib4")

  //Compruebo creaRamaHuff
  val hoja1 = HojaHuff('S', 4)
  val hoja2 = HojaHuff('O', 3)
  val hoja3 = HojaHuff('E', 2)
  val ramaHuff1 = creaRamaHuff(hoja1, hoja2)
  val ramaHuff2 = creaRamaHuff(ramaHuff1, hoja3)
  println(s"Probar crear RamaHuff: $ramaHuff1")
  println(s"Probar crear RamaHuff: $ramaHuff2")

  //Compruebo combinar
  val combinarListaHojas1 = combinar(probarDistrib1)
  val combinarListaHojas2 = combinar(probarDistrib2)
  val combinarListaHojas3 = combinar(probarDistrib3)
  val combinarListaHojas4 = combinar(probarDistrib4)
  println(s"Combinar ListaHojas1: $combinarListaHojas1")
  println(s"Combinar ListaHojas2: $combinarListaHojas2")
  println(s"Combinar ListaHojas3: $combinarListaHojas3")
  println(s"Combinar ListaHojas4: $combinarListaHojas4")

  //Compruebo esListaSingleton
  val listaSingleton1 = esListaSingleton(combinarListaHojas1)
  val listaSingleton2 = esListaSingleton(combinarListaHojas2)
  val listaSingleton3 = esListaSingleton(combinarListaHojas3)
  val listaSingleton4 = esListaSingleton(combinarListaHojas4)
  println(s"Es listaSingleton1: $listaSingleton1")
  println(s"Es listaSingleton2: $listaSingleton2")
  println(s"Es listaSingleton3: $listaSingleton3")
  println(s"Es listaSingleton4: $listaSingleton4")

  //Compruebo repetirHasta
  val repetirLista1 = repetirHasta(combinar)(esListaSingleton)(combinarListaHojas1)
  val repetirLista2 = repetirHasta(combinar)(esListaSingleton)(combinarListaHojas2)
  val repetirLista3 = repetirHasta(combinar)(esListaSingleton)(combinarListaHojas3)
  val repetirLista4 = repetirHasta(combinar)(esListaSingleton)(combinarListaHojas4)
  println(s"repetirHasta en Lista1: $repetirLista1")
  println(s"repetirHasta en Lista2: $repetirLista2")
  println(s"repetirHasta en Lista3: $repetirLista3")
  println(s"repetirHasta en Lista4: $repetirLista4")
  val repetir1Singleton = esListaSingleton(repetirLista1)
  val repetir2Singleton = esListaSingleton(repetirLista2)
  val repetir3Singleton = esListaSingleton(repetirLista3)
  val repetir4Singleton = esListaSingleton(repetirLista4)
  println(s"Es listaSingleton1: $repetir1Singleton")
  println(s"Es listaSingleton2: $repetir2Singleton")
  println(s"Es listaSingleton3: $repetir3Singleton")
  println(s"Es listaSingleton4: $repetir4Singleton")

  //Crompruebo crearArbolHuffman
  val crearMiArbol1 = ArbolHuffman("SOS ESE OSO")
  val crearMiArbol2 = ArbolHuffman(" ")
  val crearMiArbol3 = ArbolHuffman("Arbol Huffman")
  println(s"Árbol1: SOS ESE OSO")
  println(s"Árbol2: ' '")
  println(s"Árbol3: Arbol Huffman")
  println(s"Mi árbol1 creado: $crearMiArbol1")
  println(s"Mi árbol2 creado: $crearMiArbol2")
  println(s"Mi árbol3 creado: $crearMiArbol3")

  //Compruebo deArbolATabla
  val prueboArbol1 = deArbolATabla(crearMiArbol1)
  val prueboArbol2 = deArbolATabla(crearMiArbol2)
  val prueboArbol3 = deArbolATabla(crearMiArbol3)
  println(s"De Árbol1 a Tabla: $prueboArbol1")
  println(s"De Árbol2 a Tabla: $prueboArbol2")
  println(s"De Árbol3 a Tabla: $prueboArbol3")

  //Compruebo codificarTabla
  val pruebaCodificar1 = codificarTabla(prueboArbol1)("SOS ESE OSO")
  val pruebaCodificar2 = codificarTabla(prueboArbol2)(" ")
  val pruebaCodificar3 = codificarTabla(prueboArbol3)("Arbol Huffman")
  println(s"Compruebo codificarTabla con Árbol1: $pruebaCodificar1")
  println(s"Compruebo codificarTabla con Árbol2: $pruebaCodificar2")
  println(s"Compruebo codificarTabla con Árbol3: $pruebaCodificar3")

  //Compruebo decodificarTabla
  val prueboDecodificar1 = decodificarTabla(prueboArbol1)(List(0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0))
  val prueboDecodificar2 = decodificarTabla(prueboArbol2)(List())
  val prueboDecodificar3 = decodificarTabla(prueboArbol3)(List(1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0))
  println(s"Puebo decodificarTabla con Árbol1: $prueboDecodificar1")
  println(s"Puebo decodificarTabla con Árbol2: $prueboDecodificar2")
  println(s"Puebo decodificarTabla con Árbol3: $prueboDecodificar3")
}
