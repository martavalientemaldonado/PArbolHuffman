abstract class ArbolHuff (nodo : NodoHuff, rama : RamaHuff)

  def peso(arbol : ArbolHuff) : Int =
    def pesoAux(arbol: ArbolHuff, frec : Int, pos : Int) : Int = arbol match
      case arbol :: Nil => 0
      case _ => frec + pesoAux(arbol, frec, pos + 1)

    pesoAux(arbol, frec, 0)

  def caracteres (arbol : ArbolHuff) : List[Char] = arbol match
    case arbol :: Nil => Nil

  def cadenaListaChars(cadena : String) : List[Char] =


  def listaCharsCadena(listaCar : List[Char]) : String =
