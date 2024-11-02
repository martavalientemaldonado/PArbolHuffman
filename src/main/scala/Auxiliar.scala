class Auxiliar

  type Bit = 0 | 1
  type TablaCodigos = List[(Char, List[Bit])]
  
  def cadenaAListaChars(cadena : String) : List[Char] = cadena.toList

  def listaCharsACadena(listaCar : List[Char]) : String =
    def listaCharsACadenaAux(listaCar : List[Char], acc : String) : String = listaCar match
      case Nil => acc
      case head :: tail => listaCharsACadenaAux(listaCar, acc + head)
      
    listaCharsACadenaAux(listaCar, " ")