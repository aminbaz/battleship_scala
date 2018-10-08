package example

case class Game(players : List[Player]) {

}
object Game{
  //@tailrec
  def play(gamePlay : Game, numberOfGame : Int) : Game = {
    if(numberOfGame == 0){
      gamePlay
    }else{
      val r = new scala.util.Random()
      //val nameShips = List("Carrier", "Battleship", "Cruiser", "Submarine","Destroyer")
      val nameShips = List("Destroyer","Submarine")
      var player1 = gamePlay.players(0)
      var player2 = gamePlay.players(1)

      if(player1.isAI) {
        player1 = player1.copy(shipGrid = player1.shipGrid.copy(ships = ArtificialPlayer.createShips(r,nameShips, List())))
        player1 = player1.copy(shipGrid = player1.shipGrid.copy(gridFilled = Grid.placeShip(player1.shipGrid.ships,player1.shipGrid.gridFilled)))
      }else{
        println(">>>>> " + player1.name + " : Placement of ships")
        player1 = player1.copy(shipGrid = player1.shipGrid.copy(ships = player1.shipGrid.createShips(nameShips, List())))
        player1 = player1.copy(shipGrid = player1.shipGrid.copy(gridFilled = Grid.placeShip(player1.shipGrid.ships, player1.shipGrid.gridFilled)))

      }
      if(player2.isAI){
        player2 = player2.copy(shipGrid = player2.shipGrid.copy(ships = ArtificialPlayer.createShips(r,nameShips, List())))
        println("player2" + player2.shipGrid.ships)
        player2 = player2.copy(shipGrid = player2.shipGrid.copy(gridFilled = Grid.placeShip(player2.shipGrid.ships,player2.shipGrid.gridFilled)))

      }else{
        println(">>>>> " + player2.name + " : Placement of ships")
        player2 = player2.copy(shipGrid = player2.shipGrid.copy(ships = player2.shipGrid.createShips(nameShips, List())))
        player2 = player2.copy(shipGrid = player2.shipGrid.copy(gridFilled = Grid.placeShip(player2.shipGrid.ships,player2.shipGrid.gridFilled)))

      }
      val players = turnOfPlay(player1,player2,r)
      val resultGame = gamePlay.copy(players=List(players(0).copy(shipGrid = Grid(List()),shotGrid = Grid(List())),players(1).copy(score = players(1).score+1,shipGrid = Grid(List()),shotGrid = Grid(List()))))

      if(player1.isAI&&player2.isAI){
        play(resultGame, numberOfGame-1)
      }else{
        if(readLine("Replay ? (y for YES and n for NO : ")=="y"){
          play(resultGame, 1)
        }else{
          resultGame
        }
      }
    }
  }

  def turnOfPlay(player1 : Player, player2 : Player,  r : scala.util.Random) : List[Player] = {
    println(player1.shipGrid.ships)
    def checkInputShot(input : String): Option[Int] ={
      try {
        if(input.toInt<1||input.toInt>10){
          println("<!> Your value is out of bound, re-enter a value between 1 and 10")
          None
        }else{Some(input.toInt -1)}
      } catch {
        case _: Exception =>
          println("<!> Your value is not an integer")
          None
      }
    }

    def checkEndGame(ships : List[Ship]): Boolean={
      if(ships.isEmpty){
        true
      }else if(ships.head.lifePoint>0){
        false
      }else{
        checkEndGame(ships.tail)
      }
    }

    if(checkEndGame(player1.shipGrid.ships)){
      if(!player1.isAI) {println("End of the game, the winner is : "+ player2.name)}
      List(player1,player2)
    }else{
      var playerAttack = player1
      var playerDefense = player2
      var shotRow : Int = 0
      var shotCol : Int = 0
      var coordShiptouched = List(-1,-1)
      var dirShoot = List("top","right","left","bottom")
      val resultShot : (Int,Ship,Int,Int) = {
        if(playerAttack.isAI){
          //TD check the AI level
          //TD delete display for the AI
          //TD handle the shotAI
          var shotAI : (Int,Ship,Int,Int) = (0,playerDefense.shipGrid.ships.head,0,0)
          println(">>>>>>>>>> Turn of " + player1.name + "!")
          println(">>>>>>>>>> Grid of my ships : ")
          Grid.printShipGrid(playerAttack.shipGrid.gridFilled)
          //println(">>>>>>>>>> Grid of my shots : ")
          //Grid.printShipGrid(playerAttack.shotGrid.gridFilled)
          playerAttack.name match {
            case "easy" => shotAI = ArtificialPlayer.shotAIEasy(r,playerDefense.shipGrid.ships,playerDefense.shipGrid)
            case "medium" => shotAI = ArtificialPlayer.shotAIMedium(r,playerDefense.shipGrid.ships,playerDefense.shipGrid)
            case "hard" => {
              shotAI = ArtificialPlayer.shotAIHard(r,playerDefense.shipGrid.ships,playerDefense.shipGrid,coordShiptouched,dirShoot.head)
              if(shotAI._1==2){
                coordShiptouched = List(shotAI._3,shotAI._4)
              }else{
                dirShoot = dirShoot.tail
                coordShiptouched = List(-1,-1)
              }
            }
          }
          shotRow = shotAI._3
          shotCol = shotAI._4
          shotAI
        }else{
          println(">>>>>>>>>> Turn of " + player1.name + "!")
          println(">>>>>>>>>> Grid of my ships : ")
          Grid.printShipGrid(playerAttack.shipGrid.gridFilled)
          //println(">>>>>>>>>> Grid of my shots : ")
          //Grid.printShipGrid(playerAttack.shotGrid.gridFilled)
          shotRow = checkInputShot(readLine("Shoot row (value between 1 and 10) : ")).getOrElse(turnOfPlay(playerAttack,playerDefense,r)).asInstanceOf[Int]
          shotCol = checkInputShot(readLine("Shoot col (value between 1 and 10) : ")).getOrElse(turnOfPlay(playerAttack,playerDefense,r)).asInstanceOf[Int]
          val playerShoot = Player.makeShot(shotRow,shotCol,playerDefense.shipGrid.ships,playerDefense.shipGrid)
          (playerShoot._1,playerShoot._2,0,0)
        }
      }
      if(resultShot._1 == 2) {
        //TD delete update ship
        val indexShip = playerDefense.shipGrid.ships.indexOf(resultShot._2)
        val shipUpdated = playerDefense.shipGrid.ships(indexShip).copy(lifePoint =  playerDefense.shipGrid.ships(indexShip).lifePoint -1)
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(ships =  playerDefense.shipGrid.ships.updated(indexShip,shipUpdated)))
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        if(shipUpdated.lifePoint == 0 /*&& !playerAttack.isAI && !playerDefense.isAI*/) {
          println("You sank my " + shipUpdated.typeShip + " !")
        }
        turnOfPlay(playerDefense,playerAttack,r)
      }else if(resultShot._1 == -1){
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(1, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(1, List(shotRow, shotCol))))
        turnOfPlay(playerDefense,playerAttack,r)
      }else if(resultShot._1 == -2){
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(2, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(2, List(shotRow, shotCol))))
        turnOfPlay(playerDefense,playerAttack,r)
      }else{
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        turnOfPlay(playerDefense,playerAttack,r)
      }
      /*else {
        println("c'est deja passé par la ?")
        dirShoot = dirShoot.tail
        playerDefense = playerDefense.copy(shipGrid = playerDefense.shipGrid.copy(gridFilled = playerDefense.shipGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
        playerAttack = playerAttack.copy(shotGrid = playerAttack.shotGrid.copy(gridFilled = playerAttack.shotGrid.updateGrid(resultShot._1, List(shotRow, shotCol))))
      }*/
    }
  }

  def createPlayer(aiPlayer:Boolean) : Player = {
    if(aiPlayer){
      val level = readLine("Enter the level of the AI => ")
      Player(name = level,
        shipGrid = Grid(ships = List()),
        shotGrid = Grid(ships = List()),
        isAI = true)
    }else{
      val namePlayer = readLine("Player name => ")
      Player(name = namePlayer,
        shipGrid = Grid(ships = List()),
        shotGrid = Grid(ships = List()))
    }
  }
}