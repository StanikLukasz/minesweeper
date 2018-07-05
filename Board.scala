import scala.util.Random

import State._

class Board (width: Int, height: Int, mines: Int){
    var board = Array.ofDim[Field](width, height)
    var lost: Boolean = false

    {
        for (i <- 0 until width)
            for (j <- 0 until height)
                board(i)(j) = new Field
    }

    def increaseNeighbour(x: Int, y:Int){
        if(board(x)(y).state == Empty) board(x)(y).state = Number
        board(x)(y).value += 1
    }

    def prepareBoard(){
        var r = new Random
        var placeForMine = 0
        println(board(0)(0).value+"asdasdsada")
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
            case Empty => {
              handleEmpty(x,y)
              refreshBoard()
            }
            case Mine => handleMine()
            case Number => handleNumber(x,y)
        }
    }

    def handleMine(){
        lost = true;
        for(i <- 0 to width-1; j <- 0 to height-1) board(i)(j).hidden = false
    }

    def handleEmpty(x: Int, y: Int){
        board(x)(y).hidden = false
        board(x)(y).checked = true
        if(x > 0 && y > 0 && board(x-1)(y-1).state == Empty && board(x-1)(y-1).checked == false) handleEmpty(x-1,y-1)
        if(y > 0 && board(x)(y-1).state == Empty && board(x)(y-1).checked == false) handleEmpty(x,y-1)
        if(y > 0 && x < height-1 && board(x+1)(y-1).state == Empty && board(x+1)(y-1).checked == false) handleEmpty(x+1,y-1)
        if(x > 0 && board(x-1)(y).state == Empty && board(x-1)(y).checked == false) handleEmpty(x-1,y)
        if(x < height-1 && board(x+1)(y).state == Empty && board(x+1)(y).checked == false) handleEmpty(x+1,y)
        if(y < width-1 && x > 0 && board(x-1)(y+1).state == Empty && board(x-1)(y+1).checked == false) handleEmpty(x-1,y+1)
        if(y < width-1 && board(x)(y+1).state == Empty && board(x)(y+1).checked == false) handleEmpty(x,y+1)
        if(y < width-1 && x < height-1 && board(x+1)(y+1).state == Empty && board(x+1)(y+1).checked == false) handleEmpty(x+1,y+1)

        if(x > 0 && y > 0 && board(x-1)(y-1).state == Number) handleNumber(x-1,y-1)
        if(y > 0 && board(x)(y-1).state == Number) handleNumber(x,y-1)
        if(y > 0 && x < height-1 && board(x+1)(y-1).state == Number) handleNumber(x+1,y-1)
        if(x > 0 && board(x-1)(y).state == Number) handleNumber(x-1,y)
        if(x < height-1 && board(x+1)(y).state == Number) handleNumber(x+1,y)
        if(y < width-1 && x > 0 && board(x-1)(y+1).state == Number) handleNumber(x-1,y+1)
        if(y < width-1 && board(x)(y+1).state == Number) handleNumber(x,y+1)
        if(y < width-1 && x < height-1 && board(x+1)(y+1).state == Number) handleNumber(x+1,y+1)
    }

    def refreshBoard(): Unit ={
      for (i <- 0 until width)
        for (j <- 0 until height)
          board(i)(j).checked = false
    }

    def handleNumber(x: Int, y: Int){
        board(x)(y).hidden = false
    }

    def printBoard(){
        print("    ")
        for(i <- 0 until width) print("("+i+") ")
        println()
        for(i <- 0 until height){
            print("("+i+") ")
            for(j <- 0 to width-1){
                if(board(j)(i).hidden==true) print("[ ] ")
                else board(j)(i).state match{
                    case Empty => print("    ")
                    case Mine => print(" X  ")
                    case Number => print(" "+board(j)(i).value+"  ")
                }
            }
            println()
        }
    }

    def checkWin(): Boolean = {
        var won: Boolean = true
        for(i <- 0 until width){
            for(j <- 0 until height){
                if(board(i)(j).hidden==true && board(i)(j).state != Mine) won = false
                if(board(i)(j).hidden==false && board(i)(j).state == Mine) won = false
            }
        }
        return won
    }

    def checkLoose(): Boolean = {
        return lost
    }
}