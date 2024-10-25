class codificarYDecodificar
  type Bit = 0 | 1
  
  def cadenaAListaChars(cadena : String) : List[Char] =
    def cadenaAListaCharsAux(cadena : String, temp : Int) : List[Char] = 

    cadenaAListaCharsAux()

  def listaCharsACadena(listaCar : List[Char]) : String =
    def listaCharsACadenaAux(listaCar : List[Char], temp : Int) : String = listaCar match
      case Nil => 
      case _ => listaCharsACadenaAux(listaCar, temp + 1)
      
    listaCharsACadenaAux(listaCar, 0)

  def decodificar(bits : List[Bit]) : String

  def codificar(cadena : String) : List[Bit]