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
        listaCharsADistFrecAux(tail, nuevaFrecuencia)

    //Funcion para actualizar la frecuencia de un caracter
    def actualizarFrecuencia(char: Char, frecuencia: List[(Char, Int)]) : List[(Char, Int)] = frecuencia match
      case Nil => List((char,1))
      case (c, f) :: tail if c == char => (c, f + 1) :: tail
      case head :: tail => head :: actualizarFrecuencia(char, tail)
      
    listaCharsADistFrecAux(listaChar, List.empty[(Char, Int)])

      
  def distribFrecAListaHojas(frecuencias: List[(Char, Int)]): List[HojaHuff] =
    val hojas = frecuencias.map { case (char, weight) => HojaHuff(char, weight) } // Tuplas (char, Int) a hojas
    // Ordenar las hojas por peso.
    hojas.sortBy(_.weight)

  def creaRamaHuff(izq : ArbolHuffman, dch : ArbolHuffman) : RamaHuff = RamaHuff(izq, dch)

  def combinar(nodos : List[ArbolHuffman]) : List[ArbolHuffman] = nodos match
    case _ => nodos // Vacía devuelve la lista como esta
    case head :: Nil => nodos
    case head :: segundo :: tail =>  //  Extraemos dos primeros elementos
      //  Los combinamos en RamaHuff
      val nuevoArbol = RamaHuff(head, segundo)
      (nuevoArbol :: tail).sortBy(_.peso)

  def esListaSingleton(lista : List[ArbolHuffman]) : Boolean = lista.length == 1

  def repetirHasta(combinar : List[ArbolHuffman] => List[ArbolHuffman])(esListaSingleton : List[ArbolHuffman] => Boolean)(listaHojas : List[ArbolHuffman]) : List[ArbolHuffman] =
    if esListaSingleton(listaHojas) then listaHojas
    else
      val nuevaLista = combinar(listaHojas)
      repetirHasta(combinar)(esListaSingleton)(nuevaLista)

  def crearArbolHuffman(cadena: String): ArbolHuffman =
    val cadenaAChar = cadenaAListaChars(cadena)
    val listaCharATuplas = listaCharsADistFrec(cadenaAChar)
    val listaTuplasOrdenada = distribFrecAListaHojas(listaCharATuplas)
    repetirHasta(combinar)(esListaSingleton)(listaTuplasOrdenada).head
}

case class HojaHuff(char : Char, weight : Int) extends ArbolHuffman
case class RamaHuff(nodoIzq : ArbolHuffman, nodoDch : ArbolHuffman) extends ArbolHuffman

/*object ArbolHuffman{
  def apply (cadena : String) : ArbolHuffman =
    new ArbolHuffman {
      override def crearArbolHuffman(cadena: String): ArbolHuffman = super.crearArbolHuffman(cadena)
    } crearArbolHuffman(cadena)
}*/

object miPrograma extends App{
  val miArbol = RamaHuff(HojaHuff('S', 4), RamaHuff(HojaHuff('O', 3), RamaHuff(HojaHuff('E', 2), HojaHuff(' ', 2))))
  val weight = miArbol.peso
  val sec = miArbol.decodificar(List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0))
  val cod = miArbol.codificar("SOS ESE OSO")
  
  val lista = List('a', 'b', 'c', 'd', 'e', 'a', 'e', 'a', 'a')
  val resultado = miArbol.listaCharsADistFrec(lista)
  val probarDistrib = List(('O', 5), ('E', 2))
  val res = miArbol.distribFrecAListaHojas(probarDistrib)
  val listaHojas : List[ArbolHuffman] = List(HojaHuff('A', 5), HojaHuff('B', 2))
  val arbolFinal = miArbol.repetirHasta(miArbol.combinar)(miArbol.esListaSingleton)(listaHojas)

  //val crearMiArbol = ArbolHuffman("SOS ESE OSO")

  println(s"Peso: $weight")
  println(s"Cadena: $sec")
  println(s"Cadena codificada: $cod")
  println(s"Lista Char A Dist Frec: $resultado")
  println(s"Probar funcion Distrib: $res")
  println(s"Probar repetirHasta: $arbolFinal")
}
