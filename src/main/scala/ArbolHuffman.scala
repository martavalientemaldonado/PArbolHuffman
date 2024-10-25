sealed trait ArbolHuffman{
  def peso: Int = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.peso + nodoDch.peso
    case HojaHuff(char, weight) => weight

  def caracteres: List[Char] = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.caracteres ++ nodoDch.caracteres
    case HojaHuff(char, weight) => List(char)

  def decodificar(bits: List[Bit]): String =
    def decodificarAux(arbol: ArbolHuffman, bits: List[Bit], acc: String): String = this match
      case HojaHuff(char, weight) if bits.isEmpty => acc + char
      case HojaHuff(char, weight) => decodificarAux(this, bits, acc + char)
      case RamaHuff(nodoIzq, nodoDch) if bits.head == 0 => decodificarAux(nodoIzq, bits.tail, acc)
      case RamaHuff(nodoIzq, nodoDch) => decodificarAux(nodoDch, bits.tail, acc)

    decodificarAux(this, bits, " ")

  /*def codificar(cadena: String): List[Bit] =
    def codificarAux(arbol: ArbolHuffman, cadena: String, acc: List[Bit]): List[Bit] = this match
      case cadena.isEmpty => acc*/
}

case class HojaHuff(char : Char, weight : Int) extends ArbolHuffman
case class RamaHuff(nodoIzq : ArbolHuffman, nodoDch : ArbolHuffman) extends ArbolHuffman

object miPrograma extends App{
  val miArbol = RamaHuff(RamaHuff(HojaHuff('E', 2), HojaHuff(' ', 2)), RamaHuff(HojaHuff('O', 3), HojaHuff('S', 4)))
  val weight = miArbol.peso
  val sec = miArbol.decodificar(List("0100111110011011110010"))

  println(s"Peso: $weight")
  println(s"Cadena: $sec")
}
