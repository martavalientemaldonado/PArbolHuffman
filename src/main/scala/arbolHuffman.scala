trait arbolHuffman

case class HojaHuff(char : Char, peso : Int)
case class RamaHuff(nodoIzq : HojaHuff, nodoDch : HojaHuff, chars : List[Char], peso : Int)

  def peso() : Int = this match
    case RamaHuff (nodoIzq, nodoDch, chars, peso) => peso
    case HojaHuff (char, peso) => peso

  def caracteres() : List[Char] = this match
    case RamaHuff(nodoIzq, nodoDch, chars, peso) => chars
    case HojaHuff(char, peso) => char