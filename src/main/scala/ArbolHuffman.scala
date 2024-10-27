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
}

case class HojaHuff(char : Char, weight : Int) extends ArbolHuffman
case class RamaHuff(nodoIzq : ArbolHuffman, nodoDch : ArbolHuffman) extends ArbolHuffman

object miPrograma extends App{
  val miArbol = RamaHuff(RamaHuff(HojaHuff('S', 4), HojaHuff('O', 3)), RamaHuff(HojaHuff('E', 2), HojaHuff(' ', 2)))
  val weight = miArbol.peso
  val sec = miArbol.decodificar(List(1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 0, 0))
  val cod = miArbol.codificar("ESE OSO SOS")

  println(s"Peso: $weight")
  println(s"Cadena: $sec")
  println(s"Cadena codificada: $cod")
}
