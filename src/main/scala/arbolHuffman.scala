trait arbolHuffman

case class HojaHuff(char : Char, peso : Int) extends arbolHuffman
case class RamaHuff(nodoIzq : HojaHuff, nodoDch : HojaHuff) extends arbolHuffman

def peso() : Int = this match
  case RamaHuff (nodoIzq, nodoDch) => peso
  case HojaHuff (char, peso) => peso

def caracteres(): List[Char] = this match
  case RamaHuff(nodoIzq, nodoDch) => List(char)
  case HojaHuff(char, peso) => List(char)

def decodificar(bits : List[Bit]) : String =
  def decodificarAux(arbol: arbolHuffman, bits : List[Bit], acc : String) : String = this match
    case HojaHuff(char, peso) if bits.isEmpty => acc
    case HojaHuff(char, peso) => decodificarAux(this, bits, acc)
    case RamaHuff(nodoIzq, nodoDch) if bits.head == 0 => decodificarAux(nodoIzq, bits.tail, acc)
    case RamaHuff(nodoIzq, nodoDch) => decodificarAux(nodoDch, bits.tail, acc)

def codificar(cadena: String): List[Bit]


object miPrograma extends App
  val miHojaS = HojaHuff('S', 4)
  val miHojaO = HojaHuff('O', 3)
  val miHojaE = HojaHuff('E', 2)
  val miHojaEsp = HojaHuff(' ', 2)
  val rama1 = RamaHuff(miHojaE, miHojaEsp)
  val rama2 = RamaHuff(miHojaS, miHojaO)
  val miArbol = (rama1, rama2)
 val sec = miArbol.codificar("ESE OSO SOS")