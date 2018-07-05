import scala.Array._
import Console._

import State._

class Game() {
    println("Podaj wymiary i liczbe min")
    var input = scala.io.StdIn.readLine()
    var args = input.split(" ").map(_.toInt)
    var b: Board = new Board(args(0), args(1), args(2))

    b.prepareBoard()
    b.printBoard()

    var end: Boolean = false

    def play() {
        while (!end) {

            println("Podaj wspolrzedne do odsloniecia")
            var coorString = scala.io.StdIn.readLine()
            var coor = coorString.split(" ").map(_.toInt)
            b.checkField(coor(1), coor(0)) //wiersz, kolumna
            b.printBoard()
            if (b.checkWin()) {
                println("Wygrales")
                end = true
            }
            if (b.checkLoose()) {
                println("Przegrales")
                end = true
            }
        }
    }

}