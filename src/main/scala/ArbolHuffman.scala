import scala.annotation.tailrec

sealed trait ArbolHuffman{
  def peso: Int = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.peso + nodoDch.peso
    case HojaHuff(char, weight) => weight

  def caracteres: List[Char] = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.caracteres ++ nodoDch.caracteres
    case HojaHuff(char, weight) => List(char)

  def contieneCaracter(char : Char) : Boolean = this match
    case HojaHuff(c, weight) => c == char
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.contieneCaracter(char) || nodoDch.contieneCaracter(char)

  def decodificar(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(arbol: ArbolHuffman, bits: List[Bit], acc: String): String = (arbol, bits) match
      case (HojaHuff(char, weight), Nil) => acc + char //llega a una hoja donde no hay más bits y añade el char al acc
      case (HojaHuff(char, weight), _) => decodificarAux(this, bits, acc + char) //llega a una hoja con bits restantes, agrega el char al acc y reinicia la decodificacion
      case (RamaHuff(nodoIzq, nodoDch), 0 :: restoBits) => decodificarAux(nodoIzq, restoBits, acc) //si el primer bit es 0 continua por la rama izquierda y llama a decodificarAux de forma recursiva
      case (RamaHuff(nodoIzq, nodoDch), 1 :: restoBits) => decodificarAux(nodoDch, restoBits, acc) //si el primer bit es 1 continua por la rama derecha y llama a decodificarAux de forma recursiva
      case _ => acc //detiene la decodificacion y devuelve el acc

    decodificarAux(this, bits, " ")

  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(listaChar : List[Char], acc: List[Bit]): List[Bit] = listaChar match
      case Nil => acc //si la lista está vacía devuelve acc
      case char :: restoCadena => codificarAux(restoCadena, acc ++ codificarCaracteres(char, this, List.empty[Bit])) //si la lista contiene al menos un caracter, se concatena acc con codificar caracteres para obtener la secuencia de bits
      
    codificarAux(cadenaAListaChars(cadena), List.empty[Bit])
      
      
  def codificarCaracteres(char : Char, arbol : ArbolHuffman, lista : List[Bit]) : List[Bit] = arbol match
    case HojaHuff(caracter, weight) if caracter == char => lista //si nos encontramos en una hoja y contiene el caracter buscado devuelve la lista
    case RamaHuff(nodoIzq, nodoDch) =>
      if (nodoIzq.contieneCaracter(char)) codificarCaracteres(char, nodoIzq, lista :+ 0) //si el caracter se encuentra en el subarbol izquierdo, llama recursivamente a la funcion y añade 0 a la lista
      else codificarCaracteres(char, nodoDch, lista :+ 1) //si el caracter no se encuentra en el subarbol izquierdo, llama recursivamente a la funcion en el nodo derecho y añade 1 a la lista
    case _ => throw new IllegalArgumentException("Caracter no encontrado") //si el caracter no se encuentra en ninguna hoja, lanza una excepcion

  def listaCharsADistFrec(listaChar : List[Char]) : List[(Char, Int)] =
    @tailrec
    def listaCharsADistFrecAux(listaChar : List[Char], frecuencia : List[(Char, Int)]) : List[(Char, Int)] = listaChar match
      case Nil => frecuencia
      case head :: tail => listaCharsADistFrecAux(tail, actualizarFrecuencia(head, frecuencia))

    //Funcion para actualizar la frecuencia de un caracter
    def actualizarFrecuencia(char: Char, frecuencia: List[(Char, Int)]) : List[(Char, Int)] = frecuencia match
      case Nil => List((char,1))
      case (c, f) :: tail if c == char => (c, f + 1) :: tail
      case head :: tail => head :: actualizarFrecuencia(char, tail)
      
    listaCharsADistFrecAux(listaChar, List.empty[(Char, Int)])

      
  def distribFrecAListaHojas(frecuencias: List[(Char, Int)]): List[HojaHuff] =
    val hojas = frecuencias.map { case (char, weight) => HojaHuff(char, weight) } // Tuplas (char, Int) a hojas
    // Ordenar las hojas por peso.
    hojas.sortBy(_.peso)

  def creaRamaHuff(izq : ArbolHuffman, dch : ArbolHuffman) : RamaHuff = RamaHuff(izq, dch)

  def combinar(nodos : List[ArbolHuffman]) : List[ArbolHuffman] = nodos match
    case head :: segundo :: tail =>  //  Extraemos dos primeros elementos
      //  Los combinamos en RamaHuff
      val nuevoArbol = RamaHuff(head, segundo)
      (nuevoArbol :: tail).sortBy(_.peso)
    case _ => nodos // Vacía devuelve la lista como esta

  def esListaSingleton(l : List[ArbolHuffman]) : Boolean = l match
    case _ :: Nil => true
    case _ => false

  def repetirHasta(combinar : List[ArbolHuffman] => List[ArbolHuffman])(esListaSingleton : List[ArbolHuffman] => Boolean)(listaHojas : List[ArbolHuffman]) : List[ArbolHuffman] =
    if esListaSingleton(listaHojas) then listaHojas
    else repetirHasta(combinar)(esListaSingleton)(combinar(listaHojas))

  def crearArbolHuffman(cadena: String): ArbolHuffman =
    val cadenaAChar = cadenaAListaChars(cadena)
    val listaCharATuplas = listaCharsADistFrec(cadenaAChar)
    val listaTuplasOrdenada = distribFrecAListaHojas(listaCharATuplas)
    repetirHasta(combinar)(esListaSingleton)(listaTuplasOrdenada).head
}

case class HojaHuff(char : Char, weight : Int) extends ArbolHuffman
case class RamaHuff(nodoIzq : ArbolHuffman, nodoDch : ArbolHuffman) extends ArbolHuffman

object ArbolHuffman{
  def apply (cadena : String) : ArbolHuffman =
    new ArbolHuffman {
      override def crearArbolHuffman(cadena: String): ArbolHuffman = super.crearArbolHuffman(cadena)
    } crearArbolHuffman(cadena)
}

object miPrograma extends App{
  val miArbol = RamaHuff(HojaHuff('S', 4), RamaHuff(HojaHuff('O', 3), RamaHuff(HojaHuff('E', 2), HojaHuff(' ', 2))))
  val weight = miArbol.peso
  val sec = miArbol.decodificar(List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0))
  val cod = miArbol.codificar("SOS ESE OSO")
  
  val lista = List('a', 'b', 'c', 'd', 'e', 'a', 'e', 'a', 'a')
  val resultado = miArbol.listaCharsADistFrec(lista)
  val probarDistrib = List(('O', 5), ('E', 2))
  val res = miArbol.distribFrecAListaHojas(probarDistrib)
  val probarSingleton = List(('O', 5))
  val probarSingletonDistrib = miArbol.distribFrecAListaHojas(probarSingleton)
  val listaSingleton = miArbol.esListaSingleton(res)
  val listaSingleton2 = miArbol.esListaSingleton(probarSingletonDistrib)
  val listaHojas : List[ArbolHuffman] = List(HojaHuff('A', 5), HojaHuff('B', 2))
  val arbolFinal = miArbol.repetirHasta(miArbol.combinar)(miArbol.esListaSingleton)(listaHojas)

  val crearMiArbol = ArbolHuffman("SOS ESE OSO")

  println(s"Peso: $weight")
  println(s"Cadena: $sec")
  println(s"Cadena codificada: $cod")
  println(s"Lista Char A Dist Frec: $resultado")
  println(s"Probar funcion Distrib: $res")
  println(s"Es lista singleton: $listaSingleton")
  println (s"Es lista singleton 2: $listaSingleton2")
  println(s"Probar repetirHasta: $arbolFinal")
  println(s"Árbol: $crearMiArbol")
}
