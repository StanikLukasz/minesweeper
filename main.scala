import scala.Array._
import scala.util.Random
import Console._


object State extends Enumeration {
    type State = Value
    val Empty, Mine, Number = Value
}

import State._

class Field (var state: State = Empty, var value: Int = 0, var hidden: Boolean = true)

class Board (width: Int, height: Int, mines: Int){
    var board = Array.ofDim[Field](width, height)
    var lost: Boolean = false

    def increaseNeighbour(x: Int, y:Int){
        if(board(x)(y).state == Empty) board(x)(y).state = Number
        board(x)(y).value += 1
    }

    def prepareBoard(){

        var r = new Random
        var placeForMine = 0
        //println(board(0)(0).value+"asdasdsada")
        for (i <- 1 to mines){
            do{
                placeForMine = r.nextInt(height*width-1)
                //println(placeForMine)
                //println("x="+placeForMine/width+" y="+placeForMine%width)
            }
            while(board(placeForMine/width)(placeForMine%width).state!=Empty)
            //println("Preparing board")

            var x = placeForMine/width
            var y = placeForMine%width
            
            board(x)(y).state = Mine
            if(x > 0 && y > 0) increaseNeighbour(x-1,y-1)
            if(y > 0) increaseNeighbour(x,y-1)
            if(y > 0 && x < height-1) increaseNeighbour(x+1,y-1)
            if(x > 0) increaseNeighbour(x-1,y)
            if(x < height-1) increaseNeighbour(x+1,y)
            if(y < width-1 && x > 0) increaseNeighbour(x-1,y+1)
            if(y < width-1) increaseNeighbour(x,y+1)
            if(y < width-1 && x < height-1) increaseNeighbour(x+1,y+1)
        }
    }

    def checkField(x: Int, y: Int) = {
        if(!board(x)(y).hidden){
            println("Pole juz odsloniete")

        }
        else board(x)(y).state match {
            case Empty => handleEmpty(x,y)
            case Mine => endGame()
            case Number => showNumber(x,y)
        }
    }

    def endGame(){
        lost = true;
        for(i <- 0 to width-1; j <- 0 to height-1) board(i)(j).hidden = false
    }

    def handleEmpty(x: Int, y: Int){
        board(x)(y).hidden = true

        if(x > 0 && y > 0 && board(x-1)(y-1).state == Empty) handleEmpty(x-1,y-1)
        if(y > 0 && board(x)(y-1).state == Empty) handleEmpty(x,y-1)
        if(y > 0 && x < height-1 && board(x+1)(y-1).state == Empty) handleEmpty(x+1,y-1)
        if(x > 0 && board(x-1)(y).state == Empty) handleEmpty(x-1,y)
        if(x < height-1 && board(x+1)(y).state == Empty) handleEmpty(x+1,y)
        if(y < width-1 && x > 0 && board(x-1)(y+1).state == Empty) handleEmpty(x-1,y+1)
        if(y > width-1 && board(x)(y+1).state == Empty) handleEmpty(x,y+1)
        if(y > width-1 && x < height-1 && board(x+1)(y+1).state == Empty) handleEmpty(x+1,y+1)

        if(x > 0 && y > 0 && board(x-1)(y-1).state == Number) showNumber(x-1,y-1)
        if(y > 0 && board(x)(y-1).state == Number) showNumber(x,y-1)
        if(y > 0 && x < height-1 && board(x+1)(y-1).state == Number) showNumber(x+1,y-1)
        if(x > 0 && board(x-1)(y).state == Number) showNumber(x-1,y)
        if(x < height-1 && board(x+1)(y).state == Number) showNumber(x+1,y)
        if(y < width-1 && x > 0 && board(x-1)(y+1).state == Number) showNumber(x-1,y+1)
        if(y > width-1 && board(x)(y+1).state == Number) showNumber(x,y+1)
        if(y > width-1 && x < height-1 && board(x+1)(y+1).state == Number) showNumber(x+1,y+1)
    }

    def showNumber(x: Int, y: Int){
        board(x)(y).hidden = true
    }

    def printBoard(){
        println("   ")
        for(i <- 0 to width-1) println("("+i+") ")
        
        for(i <- 0 to height-1){
            println("("+i+") ")
            for(j <- 0 to width-1){
                if(board(j)(i).hidden==true) println("[ ] ")
                else board(j)(i).state match{
                    case Empty => println("   ")
                    case Mine => println("O ")
                    case Number => println(board(j)(i).value+" ")
                }
            }
        }
    }

    def checkWin(): Boolean = {
        var won: Boolean = true
        for(i <- 0 to width-1){
            for(j <- 0 to height-1){
                if(board(i)(j).hidden==true && board(i)(j).state != Mine) won = false
            }
        }
        return won
    }

    def checkLoose(): Boolean = {
        return lost
    }
}

class Game(){
    println("Podaj wymiary i liczbe min")
    var input = scala.io.StdIn.readLine()
    var args = input.split(" ").map(_.toInt)
    var b: Board = new Board(args(0), args(1), args(2))

    b.prepareBoard()
    b.printBoard()

    var end: Boolean = false
    def play(){
        while(!end){
            
            println("Podaj wspolrzedne do odsloniecia")
            var coorString = scala.io.StdIn.readLine()
            var coor = coorString.split(" ").map(_.toInt)
            b.checkField(coor(0), coor(1))
            b.printBoard()
            if(b.checkWin()){
                println("Wygrales")
                end = true
            }
            if(b.checkLoose()){
                println("Przegrales")
                end = true  
            }
        }
    }

}

object Menu{
    def startGame(){
        var newGame = new Game
        newGame.play()
    }

    def main(args: Array[String]){
        while(true){
            println("\nWitamy w grze saper")
            println("1. Rozpocznij nowa gre")
            println("2. Zakoncz dzialanie")
            var input: Int = scala.io.StdIn.readInt()
            input match{
                case 1 => startGame()
                case 2 => System.exit(0)
            }
        }
    }
}