trait arbolHuffman

  def peso : Int = this match
    case RamaHuff (nodoIzq, nodoDch) => nodoIzq.peso + nodoDch.peso
    case HojaHuff (char, peso) => peso

  def caracteres : List[Char] = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.caracteres ++ nodoDch.caracteres
    case HojaHuff(char, peso) => List(char)

  /*def decodificar(bits : List[Bit]) : String =
    def decodificarAux(arbol: arbolHuffman, bits : List[Bit], acc : String) : String = this match
      case HojaHuff(char, peso) if bits.isEmpty => acc + char
      case HojaHuff(char, peso) => decodificarAux(this, bits, acc + char)
      case RamaHuff(nodoIzq, nodoDch) if bits.head == 0 => decodificarAux(nodoIzq, bits.tail, acc)
      case RamaHuff(nodoIzq, nodoDch) => decodificarAux(nodoDch, bits.tail, acc)
    decodificarAux(this, bits, " ")

  def codificar(cadena: String): List[Bit] =
    def codificarAux(arbol : arbolHuffman, cadena : String, )*/

case class HojaHuff(char : Char, peso : Int) extends arbolHuffman
case class RamaHuff(nodoIzq : arbolHuffman, nodoDch : arbolHuffman) extends arbolHuffman

object miPrograma extends App
  val miArbol = RamaHuff(RamaHuff(HojaHuff('E', 2), HojaHuff(' ', 2)), RamaHuff(HojaHuff('O', 3), HojaHuff('S', 4)))
  val sec = miArbol.peso
  println(s"Peso: $sec")