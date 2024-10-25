trait arbolHuffman

case class HojaHuff(char : Char, peso : Int)
case class RamaHuff(nodoIzq : HojaHuff, nodoDch : HojaHuff, chars : List[Char], peso : Int)

  def peso() : Int = this match
    case RamaHuff (nodoIzq, nodoDch, chars, peso) => peso
    case HojaHuff (char, peso) => peso

  def caracteres() : List[Char] = this match
    case RamaHuff(nodoIzq, nodoDch, chars, peso) => chars
    case HojaHuff(char, peso) => List(char)

  def decodificar(bits : List[Bit]) : String =
    def decodificarAux(bits : List[Bit], acc : String) : String = this match
      case HojaHuff(char, peso) if bits.isEmpty => acc
      case HojaHuff(char, peso) => decodificarAux(bits, acc)

def codificar(cadena : String) : List[Bit]


object miPrograma extends App
  val miArbol = RamaHuff(RamaHuff(HojaHuff, HojaHuff, List("E", " "), peso = 4), RamaHuff(HojaHuff, HojaHuff, List("S", "O"), peso = 7), )