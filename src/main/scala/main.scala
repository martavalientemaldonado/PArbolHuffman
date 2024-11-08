import scala.io.StdIn.readLine

object Main extends App {

  def mostrarMenu(): Unit =
    println("   Menú Principal   ")
    println("1. Crea el árbol desde una cadena de texto")
    println("2. Calcula el peso del Árbol")
    println("3. Lista de Caracteres del Árbol")
    println("4. Decodificar usando el Árbol")
    println("5. Codificar usando el Árbol")
    println("6. Transforma el Árbol en una Tabla de Códigos")
    println("7. Codifica usando la Tabla de Códigos")
    println("8. Decodifica usando la Tabla de Códigos")
    println("9. Calcula el camino más largo del árbol")
    println("10. Salir")
    println("Seleccione una opción: ")

  var arbol: Option[ArbolHuffman] = None
  var tablaCodigos: Option[TablaCodigos] = None

  def validaCadenaBits(bits : String) : Boolean =
    var esValido = true
    for (c <- bits)
      if (c != '0' && c != '1')
        esValido = false

    esValido

  def crearArbolConTextoDado(): Unit =
    println("Introduza la cadena de texto con la que crear el Árbol: ")
    val cadena = readLine()
    arbol = Some(ArbolHuffman(cadena))
    println("Árbol creado")

  def ejecuta(opcion: Int): Unit = opcion match

    case 1 => crearArbolConTextoDado()

    case 2 => arbol match
      case None => println("Primero cree el árbol con la opción 1")
      case Some(arbol) => println(s"El peso del árbol es: ${arbol.peso}")

    case 3 => arbol match
      case None => println("Primero cree el árbol con la opción 1")
      case Some(arbol) => println(s"El árbol contiene la lista de caracteres: ${arbol.caracteres}")

    case 4 => arbol match
      case None => println("Primero cree el árbol con la opción 1")
      case Some(arbol) =>
        println("Introduzca la cadena de texto a decodificar usando el árbol (ejemplo: 01110): ")

        val bitsString = readLine()

        if (validaCadenaBits(bitsString))
          val bits : List[Bit] = bitsString.toList.map(c => if (c == '1') then 1 else 0)
          println(s"Cadena decodificada: ${arbol.decodificar(bits)}")

        else println("La cadena contiene caracteres no válidos. Solo se permiten '0' y '1'.")

    case 5 => arbol match
      case None => println("Primero cree el árbol con la opción 1")
      case Some(arbol) =>
        println("Introduzca la cadena a codificar: ")
        val cadena = readLine()
        println(s"Cadena codificada: ${arbol.codificar(cadena)}")

    case 6 => arbol match
      case None => println("Primero cree el árbol con la opción 1")
      case Some(arbol) =>
        tablaCodigos = Some(deArbolATabla(arbol))
        println("Tabla de Códigos generada")

    case 7 => tablaCodigos match
      case None => println("Primero cree el árbol con la opción 1")
      case Some(tabla) =>
        println("Introduzca la cadena de texto a codificar usando la tabla: ")
        val cadena = readLine()
        println(s"Cadena codificada: ${codificarTabla(tabla)(cadena)}")

    case 8 => tablaCodigos match
      case None => println("Primero cree el árbol con la opción 1")
      case Some(tabla) =>
        println("Introduzca la cadena de texto a decodificar usando la tabla (ejemplo: 01110): ")

        val bitsString = readLine()

        if (validaCadenaBits(bitsString))
          val bits : List[Bit] = bitsString.toList.map(c => if (c == '1') then 1 else 0)
          println(s"Cadena decodificada: ${decodificarTabla(tabla)(bits)}")

        else println("La cadena contiene caracteres no válidos. Solo se permiten '0' y '1'.")

    case 9 => arbol match
      case None => println("Primero cree el árbol con la opción 1")
      case Some(arbol) => println(s"El camino más largo del Árbol tiene longitud: ${CaminoMasLargo(arbol)}")

    case 10 => println("Saliendo...")

    case _ => println("Opción no válida. Vuelva a intentarlo")

    var salir = false

    while (!salir){
      mostrarMenu()
      val leeSeleccion = readLine()

      try{
        val opcion = leeSeleccion.toInt
        if (opcion == 10) then salir = true
        else ejecuta(opcion)
      }

      catch{
        case _: NumberFormatException => println("Opción Invalida")
      }

      println()
    }
  println("El programa ha finalizado")
}