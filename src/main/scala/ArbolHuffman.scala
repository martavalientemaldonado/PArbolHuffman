import scala.annotation.tailrec

sealed trait ArbolHuffman {
  def peso: Int = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.peso + nodoDch.peso
    case HojaHuff(char, weight) => weight

  def caracteres: List[Char] = this match
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.caracteres ++ nodoDch.caracteres // Concatenar
    case HojaHuff(char, weight) => List(char)

  def contieneCaracter(char: Char): Boolean = this match
    case HojaHuff(c, weight) => c == char
    case RamaHuff(nodoIzq, nodoDch) => nodoIzq.contieneCaracter(char) || nodoDch.contieneCaracter(char)

  def decodificar(bits: List[Bit]): String =
    @tailrec
    def decodificarAux(arbol: ArbolHuffman, bits: List[Bit], acc: String): String = (arbol, bits) match
      case (HojaHuff(char, weight), Nil) => acc + char // hoja donde no hay más bits y añade el char al acc
      case (HojaHuff(char, weight), _) => decodificarAux(this, bits, acc + char) //llega a una hoja pero quedan bits, agrega el char al acc y reinicia la decodificacion
      case (RamaHuff(nodoIzq, nodoDch), 0 :: restoBits) => decodificarAux(nodoIzq, restoBits, acc) //si el primer bit es 0 continua por la rama izquierda y llama a la funcion recursivamente
      case (RamaHuff(nodoIzq, nodoDch), 1 :: restoBits) => decodificarAux(nodoDch, restoBits, acc) //si el primer bit es 1 continua por la rama derecha y llama a la funcion recursivamente
      case _ => acc //detiene la decodificacion y devuelve el acc

    decodificarAux(this, bits, " ")

  def codificar(cadena: String): List[Bit] =
    @tailrec
    def codificarAux(listaChar: List[Char], acc: List[Bit]): List[Bit] = listaChar match
      case Nil => acc //si la lista está vacía devuelve acc
      case char :: restoCadena => codificarAux(restoCadena, acc ++ codificarCaracteres(char, this, List.empty[Bit])) //si la lista contiene al menos un carácter, se concatena acc con la función

    codificarAux(cadenaAListaChars(cadena), List.empty[Bit])


  def codificarCaracteres(char: Char, arbol: ArbolHuffman, lista: List[Bit]): List[Bit] = arbol match
    case HojaHuff(caracter, weight) if caracter == char => lista //si nos encontramos en una hoja y contiene el caracter buscado devuelve la lista
    case RamaHuff(nodoIzq, nodoDch) =>
      if (nodoIzq.contieneCaracter(char)) codificarCaracteres(char, nodoIzq, lista :+ 0) //si el caracter se encuentra en el subarbol izquierdo, llama a la funcion y añade 0 a la lista
      else codificarCaracteres(char, nodoDch, lista :+ 1) //si el caracter no se encuentra en el subarbol izquierdo, llama a la funcion en el nodo derecho y añade 1 a la lista
    case _ => throw new IllegalArgumentException("Caracter no encontrado") //si el caracter no se encuentra, lanza una excepcion

  def listaCharsADistFrec(listaChar: List[Char]): List[(Char, Int)] = //recibe una lista de caracteres y devuelve una lista de tuplas

    @tailrec
    def listaCharsADistFrecAux(listaChar: List[Char], frecuencia: List[(Char, Int)]): List[(Char, Int)] = listaChar match //recibe la lista de caracteres que aun no se han procesado y una lista de tuplas que almacena las frecuencias acumuladas
      case Nil => frecuencia //si la lista está vacía, devuelve frecuencia
      case head :: tail => listaCharsADistFrecAux(tail, actualizarFrecuencia(head, frecuencia)) //si la lista tiene al menos un elemento, llama a la funcion con el primer elemento y la lista de frecuencias actuales

    //Funcion para actualizar la frecuencia de un caracter
    def actualizarFrecuencia(char: Char, frecuencia: List[(Char, Int)]): List[(Char, Int)] = frecuencia match
      case Nil => List((char, 1)) //si la lista está vacía, este es el primer carácter. Crea una lista con la nueva tupla
      case (c, f) :: tail if c == char => (c, f + 1) :: tail //si la frecuencia tiene al menos una tupla y el carácter actual ya está, incrementa la frecuencia
      case head :: tail => head :: actualizarFrecuencia(char, tail) //si el caracter no coincide con el primero, llama recursivamente a la funcion en el resto de la lista

    listaCharsADistFrecAux(listaChar, List.empty[(Char, Int)])


  def distribFrecAListaHojas(frecuencias: List[(Char, Int)]): List[HojaHuff] =
    val hojas = frecuencias.map { case (char, weight) => HojaHuff(char, weight) } // Tuplas (char, Int) a hojas
    // Ordenar las hojas por peso.
    hojas.sortBy(_.peso)

  def creaRamaHuff(izq: ArbolHuffman, dch: ArbolHuffman): RamaHuff = RamaHuff(izq, dch)

  def combinar(nodos: List[ArbolHuffman]): List[ArbolHuffman] = nodos match
    case head :: segundo :: tail => //  Extraemos dos primeros elementos
      //  Los combinamos en RamaHuff
      val nuevoArbol = RamaHuff(head, segundo)
      (nuevoArbol :: tail).sortBy(_.peso)
    case _ => nodos // Vacía devuelve la lista como esta

  def esListaSingleton(lista: List[ArbolHuffman]): Boolean = lista match
    case _ :: Nil => true //si la lista solo tiene un elemento devuelve true
    case _ => false //en caso contrario, devuelve false

  def repetirHasta(combinar: List[ArbolHuffman] => List[ArbolHuffman])(esListaSingleton: List[ArbolHuffman] => Boolean)(listaHojas: List[ArbolHuffman]): List[ArbolHuffman] =
    if listaHojas == Nil then listaHojas
    else if esListaSingleton(listaHojas) then listaHojas //si tiene un elemento devuelve la lista
    else repetirHasta(combinar)(esListaSingleton)(combinar(listaHojas)) //si tiene más de un elemento, llama a la función hasta que tenga uno

  def crearArbolHuffman(cadena: String): ArbolHuffman =
    val cadenaAChar = cadenaAListaChars(cadena) //convierte la cadena de texto en una cadena de carácteres
    val listaCharATuplas = listaCharsADistFrec(cadenaAChar) //convierte la lista de caracteres en una lista de tuplas
    val listaTuplasOrdenada = distribFrecAListaHojas(listaCharATuplas) //ordena la lista de tuplas por peso
    repetirHasta(combinar)(esListaSingleton)(listaTuplasOrdenada).head //construye el árbol con las tuplas ordenadas por peso

  // Convertir arbol codificacion en tabla de codificacion
  def deArbolATabla(arbol: ArbolHuffman): TablaCodigos =
    def deArbolATablaAux(arbol: ArbolHuffman, bits: List[Bit]): TablaCodigos = arbol match
      case HojaHuff(char, weight) => List((char, bits)) // Si es hoja crea la lista de tuplas
      case RamaHuff(nodoIzq, nodoDch) => deArbolATablaAux(nodoIzq, bits :+ 0) ++ deArbolATablaAux(nodoDch, bits :+ 1) // Lo hace otra vez con nodo izquierdo añadiendo bit=0 y con la dcha con bit=1

    deArbolATablaAux(arbol, List.empty[Bit])

  def codificarTabla(tabla: TablaCodigos)(cadena: String): List[Bit] =
    @tailrec
    def codificarCaracter(tabla: TablaCodigos, char: Char): List[Bit] = tabla match // Para un caracter
      case Nil => List.empty[Bit] // Devuelve lista vacia de bits
      case (c, bits) :: tail if c == char => bits // Si encuentra ese caracter que devuelva los bits de la tupla
      case _ :: tail => codificarCaracter(tail, char) // Si no lo encuentra que siga
    def codificarCadena(cadena: List[Char]): List[Bit] = cadena match // Para toda la cadena
      case Nil => List.empty[Bit] // Si la cadena está vacía devuelve lista vacía de bits3e
      case char :: tail =>
        val bits = codificarCaracter(tabla, char) // Obtiene los bits del carácter
        bits ++ codificarCadena(tail) // Concatena los bits y sigue con la parte de la cadena que queda
    codificarCadena(cadena.toList)

  def decodificarTabla(Tabla: TablaCodigos)(bitsDados: List[Bit]): String =
    def decodificarCaracter(tabla: TablaCodigos, bitsDados: List[Bit]): Char = tabla match {
      case Nil => throw new NoSuchElementException("No lo hemos encontrado en la tabla")// Si la tabla está vacía, no hay correspondencia
      case (c, bits) :: tail if bitsDados == bits => c // Si los bits coinciden, devuelve el carácter
      case _ :: tail => decodificarCaracter(tail, bitsDados) // Busca en el resto de la tabla

    def decodificarCadena(bits: List[Bit], acc: String): String = bits match
      case Nil => acc // Si no quedan bits devuelve el acumulador
      case bitsDados :: resto =>
        val (caracter, numBits) = decodificarCaracter(tabla, bits)


    }

  case class HojaHuff(char: Char, weight: Int) extends ArbolHuffman

  case class RamaHuff(nodoIzq: ArbolHuffman, nodoDch: ArbolHuffman) extends ArbolHuffman

  object ArbolHuffman {
    def apply(cadena: String): ArbolHuffman =
      new ArbolHuffman {
        override def crearArbolHuffman(cadena: String): ArbolHuffman = super.crearArbolHuffman(cadena)
      } crearArbolHuffman (cadena)
  }

  object miPrograma extends App {

    //Crea el árbol
    val miArbol = RamaHuff(HojaHuff('S', 4), RamaHuff(HojaHuff('O', 3), RamaHuff(HojaHuff('E', 2), HojaHuff(' ', 2))))
    println(s"Mi Árbol: $miArbol")

    //Calcula el peso del árbol
    val weight = miArbol.peso
    println(s"Peso: $weight")

    //Cojo los caracteres del árbol
    val caracter = miArbol.caracteres
    println(s"Caracteres: $caracter")

    //Decodifica la lista
    val sec = miArbol.decodificar(List(0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0))
    println(s"Cadena decodificada: $sec")

    //Codifica la cadena
    val cod = miArbol.codificar("SOS ESE OSO")
    println(s"Cadena codificada: $cod")

    //Crea una lista de prueba
    val lista1 = List('a', 'b', 'c', 'd', 'e')
    val lista2 = List()
    val lista3 = List('a', 'b', 'a', 'c', 'a', 'a', 'd', 'e', 'b', 'c', 'a', 'b')
    val lista4 = List('a', 'b')
    println(s"Lista1: $lista1")
    println(s"Lista2: $lista2")
    println(s"Lista3: $lista3")
    println(s"Lista4: $lista4")

    //Compruebo la función listaCharsADistFrec
    val comprueboLista1 = miArbol.listaCharsADistFrec(lista1)
    val comprueboLista2 = miArbol.listaCharsADistFrec(lista2)
    val comprueboLista3 = miArbol.listaCharsADistFrec(lista3)
    val comprueboLista4 = miArbol.listaCharsADistFrec(lista4)
    println(s"Lista Char A Dist Frec Lista1: $comprueboLista1")
    println(s"Lista Char A Dist Frec Lista2: $comprueboLista2")
    println(s"Lista Char A Dist Frec Lista3: $comprueboLista3")
    println(s"Lista Char A Dist Frec Lista4: $comprueboLista4")

    //Compruebo la función distribFrecAListaHojas
    val probarDistrib1 = miArbol.distribFrecAListaHojas(comprueboLista1)
    val probarDistrib2 = miArbol.distribFrecAListaHojas(comprueboLista2)
    val probarDistrib3 = miArbol.distribFrecAListaHojas(comprueboLista3)
    val probarDistrib4 = miArbol.distribFrecAListaHojas(comprueboLista4)
    println(s"Probar funcion Distrib Lista1: $probarDistrib1")
    println(s"Probar funcion Distrib Lista2: $probarDistrib2")
    println(s"Probar funcion Distrib Lista3: $probarDistrib3")
    println(s"Probar funcion Distrib Lista4: $probarDistrib4")

    //Compruebo creaRamaHuff
    val hoja1 = HojaHuff('S', 4)
    val hoja2 = HojaHuff('O', 3)
    val hoja3 = HojaHuff('E', 2)
    val ramaHuff1 = miArbol.creaRamaHuff(hoja1, hoja2)
    val ramaHuff2 = miArbol.creaRamaHuff(ramaHuff1, hoja3)
    println(s"Probar crear RamaHuff: $ramaHuff1")
    println(s"Probar crear RamaHuff: $ramaHuff2")

    //Compruebo combinar
    val combinarListaHojas1 = miArbol.combinar(probarDistrib1)
    val combinarListaHojas2 = miArbol.combinar(probarDistrib2)
    val combinarListaHojas3 = miArbol.combinar(probarDistrib3)
    val combinarListaHojas4 = miArbol.combinar(probarDistrib4)
    println(s"Combinar ListaHojas1: $combinarListaHojas1")
    println(s"Combinar ListaHojas2: $combinarListaHojas2")
    println(s"Combinar ListaHojas3: $combinarListaHojas3")
    println(s"Combinar ListaHojas4: $combinarListaHojas4")

    //Compruebo esListaSingleton
    val listaSingleton1 = miArbol.esListaSingleton(combinarListaHojas1)
    val listaSingleton2 = miArbol.esListaSingleton(combinarListaHojas2)
    val listaSingleton3 = miArbol.esListaSingleton(combinarListaHojas3)
    val listaSingleton4 = miArbol.esListaSingleton(combinarListaHojas4)
    println(s"Es listaSingleton1: $listaSingleton1")
    println(s"Es listaSingleton2: $listaSingleton2")
    println(s"Es listaSingleton3: $listaSingleton3")
    println(s"Es listaSingleton4: $listaSingleton4")

    //Compruebo repetirHasta
    val repetirLista1 = miArbol.repetirHasta(miArbol.combinar)(miArbol.esListaSingleton)(combinarListaHojas1)
    val repetirLista2 = miArbol.repetirHasta(miArbol.combinar)(miArbol.esListaSingleton)(combinarListaHojas2)
    val repetirLista3 = miArbol.repetirHasta(miArbol.combinar)(miArbol.esListaSingleton)(combinarListaHojas3)
    val repetirLista4 = miArbol.repetirHasta(miArbol.combinar)(miArbol.esListaSingleton)(combinarListaHojas4)
    println(s"repetirHasta en Lista1: $repetirLista1")
    println(s"repetirHasta en Lista2: $repetirLista2")
    println(s"repetirHasta en Lista3: $repetirLista3")
    println(s"repetirHasta en Lista4: $repetirLista4")
    val repetir1Singleton = miArbol.esListaSingleton(repetirLista1)
    val repetir2Singleton = miArbol.esListaSingleton(repetirLista2)
    val repetir3Singleton = miArbol.esListaSingleton(repetirLista3)
    val repetir4Singleton = miArbol.esListaSingleton(repetirLista4)
    println(s"Es listaSingleton1: $repetir1Singleton")
    println(s"Es listaSingleton2: $repetir2Singleton")
    println(s"Es listaSingleton3: $repetir3Singleton")
    println(s"Es listaSingleton4: $repetir4Singleton")

    //Crompruebo crearArbolHuffman
    val crearMiArbol1 = ArbolHuffman("SOS ESE OSO")
    val crearMiArbol2 = ArbolHuffman(" ")
    val crearMiArbol3 = ArbolHuffman("Arbol Huffman")
    println(s"Árbol1: SOS ESE OSO")
    println(s"Árbol2: ' '")
    println(s"Árbol3: Arbol Huffman")
    println(s"Mi árbol1 creado: $crearMiArbol1")
    println(s"Mi árbol2 creado: $crearMiArbol2")
    println(s"Mi árbol3 creado: $crearMiArbol3")

    //Compruebo deArbolATabla
    val prueboArbol1 = miArbol.deArbolATabla(crearMiArbol1)
    val prueboArbol2 = miArbol.deArbolATabla(crearMiArbol2)
    val prueboArbol3 = miArbol.deArbolATabla(crearMiArbol3)
    println(s"De Árbol1 a Tabla: $prueboArbol1")
    println(s"De Árbol2 a Tabla: $prueboArbol2")
    println(s"De Árbol3 a Tabla: $prueboArbol3")
  }
}
