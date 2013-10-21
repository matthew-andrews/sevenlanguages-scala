/**
 * TIC TAC TOE
 *
 * Heavily influenced by Jim (http://brikis98.blogspot.co.uk/2012/03/seven-languages-in-seven-weeks-scala.html)
 *
 * @author Matt Andrews <code@mattandre.ws>
 */

object Marker extends Enumeration {
  type Marker = Value
  val EMPTY = Value("_")
  val X = Value("x")
  val O = Value("o")
}

import Marker._

class Board(size: Int) {

    // The current board's state
    var board = List.tabulate(size, size) { (m, n) => EMPTY }

    override def toString() = board.map{
      _.mkString(", ")
    }.mkString("\n")

    def rows() = board

    def cols() = board.transpose

    def diags() = List.tabulate(2, size) { (m, n) =>
        if (m == 0) board(n)(n) else board(size - 1 - n)(n)
    }

    def spacer(value: String) = " " * (size.toString.length() - value.length() + 2)

    def boardFull() = board.flatten.count(_ == EMPTY) == 0

    def allEqual(elements: List[Marker]) = !elements.contains(EMPTY) && elements.distinct.size == 1

    def anyListAllEqual(lists: List[List[Marker]]): Boolean = lists.foldLeft(false)(_ || allEqual(_))

    def boardWon() = anyListAllEqual(rows()) || anyListAllEqual(cols()) || anyListAllEqual(diags())

    def placeMarker(index: Int, marker: Marker): Boolean = {
      val x = index % size
      val y = (index - x) / size
      if (x >= 0 && index < (size * size) && board(x)(y) == EMPTY) {
        println("(" + x + "," + y + ")")
        board = List.tabulate(size, size) { (m, n) =>
          if (x == m && y == n) marker else board(m)(n)
        }
        return true
      }
      
      false
    }
}

val game = new Board(3)
var currentMove = O
while (!game.boardFull() && !game.boardWon()) {
  currentMove = if (currentMove == X) O else X
  println("\nThe board:\n")
  println(game)
  print("\nIt's " + currentMove + "'s turn! Enter a location: ")
 
  val index = Console.readInt()
  if (!game.placeMarker(index, currentMove)) {
    println("Invalid location!")    
  }
}

println()
 
if (game.boardWon()) {
  println("Player " + currentMove + " wins!")
} else {
  println("It's a draw!")
}
 
println()
println(game)
