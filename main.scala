import Console._

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