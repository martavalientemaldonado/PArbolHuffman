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
      case (HojaHuff(char, weight), Nil) => acc + char
      case (HojaHuff(char, weight), _) => decodificarAux(this, bits, acc + char)
      case (RamaHuff(nodoIzq, nodoDch), 0 :: restoBits) => decodificarAux(nodoIzq, restoBits, acc)
      case (RamaHuff(nodoIzq, nodoDch), 1 :: restoBits) => decodificarAux(nodoDch, restoBits, acc) 
      case _ => acc

    decodificarAux(this, bits, " ")

  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(listaChar : List[Char], acc: List[Bit]): List[Bit] = listaChar match
      case Nil => acc
      case char :: restoCadena => codificarAux(restoCadena, acc ++ codificarCaracteres(char, this, List.empty[Bit]))
      
    codificarAux(cadenaAListaChars(cadena), List.empty[Bit])
      
      
  def codificarCaracteres(char : Char, arbol : ArbolHuffman, lista : List[Bit]) : List[Bit] = arbol match
    case HojaHuff(caracter, weight) if caracter == char => lista
    case RamaHuff(nodoIzq, nodoDch) =>
      if (nodoIzq.contieneCaracter(char)) codificarCaracteres(char, nodoIzq, lista :+ 0)
      else codificarCaracteres(char, nodoDch, lista :+ 1)
    case _ if contieneCaracter(char) == false => throw new IllegalArgumentException("Caracter no encontrado")

  def listaCharsADistFrec(listaChar : List[Char]) : List[(Char, Int)] =
    @tailrec
    def listaCharsADistFrecAux(listaChar : List[Char], frecuencia : List[(Char, Int)]) : List[(Char, Int)] = listaChar match
      case Nil => frecuencia
      case head :: tail => 
        val nuevaFrecuencia = actualizarFrecuencia(head, frecuencia)
        listaCharsADistFrecAux(listaChar, nuevaFrecuencia)

    //Funcion para actualizar la frecuencia de un caracter
    def actualizarFrecuencia(char: Char, frecuencia: List[(Char, Int)]) : List[(Char, Int)] = frecuencia match
      case Nil => List((char,1))
      case (c, f) :: tail if c == char => (c, f + 1) :: tail
      case head :: tail => head :: actualizarFrecuencia(char, tail)
      
    listaCharsADistFrecAux(listaChar, Nil)

      
  def distribFrecAListaHojas(frecuencias: List[(Char, Int)]): List[HojaHuff] =
    val hojas = frecuencias.map { case (char, weight) => HojaHuff(char, weight) } // Tuplas (char, Int) a hojas
    // Ordenar las hojas por peso. No se si vale usar sortBy
    hojas.sortBy(_.weight)

  def creaRamaHuff(izq : ArbolHuffman, dch : ArbolHuffman) : RamaHuff = RamaHuff(izq, dch)

  def combinar(nodos : List[ArbolHuffman]) : List[ArbolHuffman] = this match
    case Nil => nodos // Vacía devuelve la lista como esta
    case head :: Nil => nodos // 1 elemento devuelve lista como esta
    case head :: segundo :: tail =>
      //  Extraemos dos primeros elementos
      //  Los combinamos en RamaHuff
      val nuevoArbol = RamaHuff(head, segundo)

  def esListaSingleton(lista : List[ArbolHuffman]) : Boolean =
    if lista.length == 1 then true
    else false

  def repetirHasta(combinado : List[ArbolHuffman], esListaSingleton : Boolean)(listaHojas : List[ArbolHuffman]) : List[ArbolHuffman] =
    if (esListaSingleton == true) then listaHojas
    else
      val lista = combinar(listaHojas)
      repetirHasta(combinado, esListaSingleton)(lista)

  /*def crearArbolHuffman(cadena: String): ArbolHuffman = caracteres match
    case Nil => throw new Exception("No se crea el arbol porque la lista de caracteres está vacía")
    case head :: Nil => HojaHuff(caracteres.head, caracteres.head.weight)
    case head :: tail => crearArbolHuffman(cadena.tail)*/
}

case class HojaHuff(char : Char, weight : Int) extends ArbolHuffman
case class RamaHuff(nodoIzq : ArbolHuffman, nodoDch : ArbolHuffman) extends ArbolHuffman

object miPrograma extends App{
  val miArbol = RamaHuff(HojaHuff('S', 4), RamaHuff(HojaHuff('O', 3), RamaHuff(HojaHuff('E', 2), HojaHuff(' ', 2))))
  val weight = miArbol.peso
  val sec = miArbol.decodificar(List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0))
  val cod = miArbol.codificar("SOS ESE OSO")
  
  val lista = List('a', 'b', 'c', 'd', 'e')
  //val resultado = miArbol.listaCharsADistFrec(lista)
  val probarDistrib = List(('O', 5), ('E', 2))
  val res = miArbol.distribFrecAListaHojas(probarDistrib)

  println(s"Peso: $weight")
  println(s"Cadena: $sec")
  println(s"Cadena codificada: $cod")
  //println(s"Lista Char A Dist Frec: $resultado")
  println(s"Probar funcion Distrib: $res")
}
