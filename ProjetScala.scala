package main

/*
Creer un class "Tondeuse" avec :
    - Atributs:
        - Position initial "x" et "y"
        - Orientation initial "or"
    - Méthode:
        - Avancer sur la surface
*/


class Tondeuse ( var x: Int, var y: Int, var or: String) { /* Constructeur */

  // Je définis la méthode "avancer" pour la classe Tondeuse avec les paramètres:
  // la limite horizontale de la surface "limitX", la limite verticale "limitY" et la chaîne avec les instructions de motivation "inst"

  def avancer ( limitX: Int, limitY: Int, inst: String ) {

    // J'utilise une exception pour arrêter le programme si la Tondeuse part de l'extérieur de la surface
    object TondeuseEnDehors extends Exception { }

      // J'utilise un Try pour pouvoir intercepter l'exception au cas où la Tondeuse démarre en dehors de la surface
      try {

        // Pour chaque caractère des instructions:
        for (i <- inst) {

          // Si la tondeuse est dans les limites de la surface:
          if ( 0 < x && x < limitX && 0 < y && y < limitY ) {

            if (s"$i" == "A") { /* Instruction d' Avancer en fonction de l'orientation */
              if      (or == "N") y = y + 1 // vers le nord
              else if (or == "E") x = x + 1 // vers l'est
              else if (or == "W") x = x - 1 // vers l'ouest
              else if (or == "S") y = y - 1 // vers le sud
              else println(s"L'orientation '$or' n'est pas valide: valeur different de N, E, W et S")

            } else if (s"$i" == "G") { /* Instruction de turner vers la gauche en fonction de l'orientation */
              if      (or == "N") or = "W" // nord : ouest
              else if (or == "E") or = "N" // est : nord
              else if (or == "W") or = "S" // ouest : sud
              else if (or == "S") or = "E" // sud : est
              else println(s"L'orientation '$or' n'est pas valide: valeur different de N, E, W et S")

            } else if (s"$i" == "D") { /* Instruction de turner vers la droite en fonction de l'orientation */
              if      (or == "N") or = "E" // nord : est
              else if (or == "E") or = "S" // est : sud
              else if (or == "W") or = "N" // ouest : nord
              else if (or == "S") or = "W" // sud : ouest
              else println(s"L'orientation '$or' n'est pas valide: valeur different de N, E, W et S")

            } else println(s"L'instruction '$i' n'est pas valide: valuer differente de A, G et D")

          // Si la tondeuse est sur les limites de la surface:
          } else if ( ( x == 0      && y >= 0 && y <= limitY ) ||
                      ( x == limitX && y >= 0 && y <= limitY ) ||
                      ( y == 0      && x >= 0 && x <= limitX ) ||
                      ( y == limitY && x >= 0 && x <= limitX )) {

            /* Instruction d' Avancer -> AVEC DES RESTRICTIONS car le fait d'être sur le limite pourrait le faire sortir de la surface  */
            if (s"$i" == "A") {

              if (or == "N") { // vers le nord
                if (y == limitY) println("En dehors des limites par le nord: tondeuse bouge pas")
                else y = y + 1
              } else if (or == "E") { // vers l'est
                if (x == limitX) println("En dehors des limites par la droite: tondeuse bouge pas")
                else x = x + 1
              } else if (or == "W") { // vers l'ouest
                if (x == 0) println("En dehors des limites par la gauche: tondeuse bouge pas")
                else x = x - 1
              } else if (or == "S") { // vers le sud
                if (y == 0) println("En dehors des limites par la gauche: tondeuse bouge pas")
                else y = y - 1
              } else println(s"L'orientation '$or' n'est pas valide: valeur different de N, E, W et S")

            /* Instruction de turner vers la gauche */
            } else if (s"$i" == "G") {

              if      (or == "N") or = "W" // nord : ouest
              else if (or == "E") or = "N" // est : nord
              else if (or == "W") or = "S" // ouest : sud
              else if (or == "S") or = "E" // sud : est
              else println(s"L'orientation '$or' n'est pas valide: valeur different de N, E, W et S")

            /* Instruction de turner vers la droite */
            } else if (s"$i" == "D") {

              if      (or == "N") or = "E" // nord : est
              else if (or == "E") or = "S" // est : sud
              else if (or == "W") or = "N" // ouest : nord
              else if (or == "S") or = "W" // sud : ouest
              else println(s"L'orientation '$or' n'est pas valide: valeur different de N, E, W et S")

            } else println(s"L'instruction '$i' n'est pas valide: valuer differente de A, G et D")
          };

          // Si la tondeuse démarre en déhors de la surface on declare exception pour arreter le programme
          if ( x < 0 || y < 0 || x > limitX || y > limitY ) throw TondeuseEnDehors
        }
    } catch { case TondeuseEnDehors => println("La Tondeuse démarre en déhors de la surface: tondeuse bouge pas")}
  }
}



object ProjetScala extends App {

  // Test avec le fichier:
  var fichier = Array("5 5" , "1 2 N", "GAGAGAGAA", "3 3 E", "AADAADADDA")

  // Ici on trouve la quantité de tondeuses en question
  val nb_tondeuses = ( fichier.size - 1) / 2

  // Pour chaque tondeuse faire:
  for (i <- 1 to nb_tondeuses ) {

    // Créer l'instance de la classe Tondeuse avec Position initial "x" et "y" et Orientation initial "or"
    val t = new Tondeuse(x = fichier(1+(2*(i-1))).charAt(0).asDigit, y = fichier(1+(2*(i-1))).charAt(2).asDigit, or = s"${fichier(1+(2*(i-1))).charAt(4)}")

    // Implémenter la méthode "avancer" de la class Tondeuse
    t.avancer(limitX = fichier(0).charAt(0).asDigit, limitY = fichier(0).charAt(2).asDigit, s"${fichier(i*2)}")

    println(s"Tondeuse $i: ${t.x} ${t.y} ${t.or}")

  }
}












