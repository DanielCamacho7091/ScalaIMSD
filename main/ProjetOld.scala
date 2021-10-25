package main

/*   Projet Scala - 2021/2022

La société MowItNow a décidé de développer une tondeuse à gazon automatique, destinée aux surfaces rectangulaires.

La tondeuse peut être programmée pour parcourir l'intégralité de la surface. La position de la tondeuse est
représentée par une combinaison de coordonnées (x,y) et d'une lettre indiquant l'orientation selon la notation
cardinale anglaise (N,E,W,S). La pelouse est divisée en grille pour simplifier la navigation.

Par exemple, la position de la tondeuse peut être « 0, 0, N », ce qui signifie qu'elle se situe dans le coin
inférieur gauche de la pelouse, et orientée vers le Nord. Pour contrôler la tondeuse, on lui envoie une séquence
simple de lettres. Les lettres possibles sont « D », « G » et « A ».
« D » et « G » font pivoter la tondeuse de 90° à droite ou à gauche respectivement, sans la déplacer.
« A » signifie que l'on avance la tondeuse d'une case dans la direction à laquelle elle fait face, et sans
modifier son orientation.

Si la position après mouvement est en dehors de la pelouse, la tondeuse ne bouge pas, conserve son orientation
et traite la commande suivante. On assume que la case directement au Nord de la position (x, y) a pour coordonnées
(x, y+1).

Pour programmer la tondeuse, on lui fournit un fichier d'entrée construit comme suit :
  • La première ligne correspond aux coordonnées du coin supérieur droit de la pelouse,
  celles du coin inférieur gauche sont supposées être (0,0).
  • La suite du fichier permet de piloter toutes les tondeuses qui ont été déployées.
  Chaque tondeuse a deux lignes la concernant :
    - la première ligne donne la position initiale de la tondeuse, ainsi que son
    orientation. La position et l'orientation sont fournies sous la forme de 2 chiffres
    et une lettre, séparés par un espace.
    - la seconde ligne est une série d'instructions ordonnant à la tondeuse d'explorer
    la pelouse. Les instructions sont une suite de caractères sans espaces.

  Chaque tondeuse se déplace de façon séquentielle, ce qui signifie que la seconde tondeuse ne bouge que lorsque
  la première a exécuté intégralement sa série d'instructions.
  Lorsqu'une tondeuse achève une série d'instruction, elle communique sa position et son orientation.

  OBJECTIF
  Concevoir et écrire un programme en Scala, implémentant la spécification ci-dessus et passant le test ci-après.

  TEST
  Le fichier suivant est fourni en entrée :

5 5
1 2 N
GAGAGAGAA
3 3 E
AADAADADDA

  On attend le résultat suivant (position finale des tondeuses) :
  Tondeuse 1 : 1 3 N
  Tondeuse 2 : 5 1 E  */


/* LIEN A SUIVRE:

https://docs.scala-lang.org/tour/classes.html


MIRAR LO DE TRY Y LO DE CAPTURAR ERROR -> OUT OF BOUNDS
MIRAR TAMBIEN EL TEXTO DE JAVA EN PAGINA 3

GUIA EN SLIDE 6!!!!!


usar esto para meter manejo de errores etc https://www.scala-lang.org/api/2.13.6/scala/util/Either.html

 */




import scala.io.Source

// Creer un class "Tondeuse" avec :

// - Atributs: La position ( x , y ) et l'orientation initial (or)
// - Methode: Avancer sur la surface


class Tondeuse ( var x: Int, var y: Int, var or: String) { /* Constructeur */

  def avancer(limitX: Int, limitY: Int, inst: String) {

    object TondeuseEnDehors extends Exception { } /* Si le fichier place la Tondeuse en dehors dès le debut on arrete */

      try {

        // Pour chaque caractere des instructions faire le suivant:
        for (i <- inst) {

          // Si la tondeuse est dans les limites de la surface
          if ( 0 < x && x < limitX && 0 < y && y < limitY ) {

                if (s"$i" == "A") /* Instruction d' Avancer */
                  if (or == "N") y = y + 1 // vers le nord
                  else if (or == "E") x = x + 1 // vers l'est
                  else if (or == "W") x = x - 1 // vers l'ouest
                  else y = y - 1 // vers le sud
                else if (s"$i" == "G") /* Instruction de turner vers la gauche */
                  if (or == "N") or = "W" // nord : ouest
                  else if (or == "E") or = "N" // est : nord
                  else if (or == "W") or = "S" // ouest : sud
                  else or = "E" // sud : est
                else if (s"$i" == "D") /* Instruction de turner vers la droite */
                  if (or == "N") or = "E" // nord : est
                  else if (or == "E") or = "S" // est : sud
                  else if (or == "W") or = "N" // ouest : nord
                  else or = "W" // sud : ouest

          // Si la tondeuse est sur les limites de la surface
          } else if ( ( x == 0      && y >= 0 && y <= limitY ) ||
                      ( x == limitX && y >= 0 && y <= limitY ) ||
                      ( y == 0      && x >= 0 && x <= limitX ) ||
                      ( y == limitY && x >= 0 && x <= limitX )) {

                if (s"$i" == "A") {  /* Instruction d' Avancer -> AVEC DES RESTRICTIONS */

                      if (or == "N") { // vers le nord
                        if (y == limitY) println("Out of bounds pour le nord -> tondeuse bouge pas")
                        else y = y + 1
                      } else if (or == "E") { // vers l'est
                        if (x == limitX) println("Out of bounds pour la droite -> tondeuse bouge pas")
                        else x = x + 1
                      } else if (or == "W") { // vers l'ouest
                        if (x == 0) println("Out of bounds la gauche -> tondeuse bouge pas")
                        else x = x - 1
                      } else if (or == "S") { // vers le sud
                        if (y == 0) println("Out of bounds pour le sud -> tondeuse bouge pas")
                        else y = y - 1
                      }

                } else if (s"$i" == "G") { /* Instruction de turner vers la gauche */

                      if (or == "N") or = "W" // nord : ouest
                      else if (or == "E") or = "N" // est : nord
                      else if (or == "W") or = "S" // ouest : sud
                      else or = "E" // sud : est

                } else if (s"$i" == "D") { /* Instruction de turner vers la droite */

                      if (or == "N") or = "E" // nord : est
                      else if (or == "E") or = "S" // est : sud
                      else if (or == "W") or = "N" // ouest : nord
                      else or = "W" // sud : ouest

            }
          };
          // Si la tondeuse démarre en déhors de la surface on declare exception pour arreter le programme
          if ( x < 0 || y < 0 || x > limitX || y > limitY ) throw TondeuseEnDehors
        }
      } catch { case TondeuseEnDehors => println("La Tondeuse demarre en dehors de la surface -> tondeuse bouge pas")}
    }
  }



object Projet extends App {

  /* Lire le fichier */
  val fichier = Source.fromFile("C:\\Users\\imsd\\Documents\\Scala\\fichierTest.txt").getLines.toArray

  /* Créer d'instances tondeuse 1 et 2 de la class "Tondeuse" */
  val t1 = new Tondeuse( x = fichier(1).charAt(0).asDigit, y = fichier(1).charAt(2).asDigit, or = s"${fichier(1).charAt(4)}" )
  val t2 = new Tondeuse( x = fichier(3).charAt(0).asDigit, y = fichier(3).charAt(2).asDigit, or = s"${fichier(3).charAt(4)}" )

  t1.avancer( limitX = fichier(0).charAt(0).asDigit , limitY = fichier(0).charAt(2).asDigit , s"${fichier(2)}")
  t2.avancer( limitX = fichier(0).charAt(0).asDigit , limitY = fichier(0).charAt(2).asDigit , s"${fichier(4)}")

  println(s"Tondeuse 1: ${t1.x} ${t1.y} ${t1.or}")
  println(s"Tondeuse 2: ${t2.x} ${t2.y} ${t2.or}")

}















