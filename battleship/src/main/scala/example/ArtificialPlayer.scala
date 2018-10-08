package example

case class ArtificialPlayer(level : String, shipGrid : Grid, shotGrid : Grid, score : Int = 0) {

}
object ArtificialPlayer{
  //TD rewrite
  def createShips(r : scala.util.Random, listNameShips : List[String], ships : List[Ship]) : List[Ship] = {
    if(listNameShips.isEmpty){
      ships
    }else{
      val direction = if(r.nextInt() % 2 == 0){"h"}else{"v"}
      val ship = Ship.checkPlacement(listNameShips.head,r.nextInt(10),r.nextInt(10),direction,ships)
      ship match {
        case None => createShips(r, listNameShips, ships)
        case _ => {
          createShips(r, listNameShips.tail, ships :+ ship.get)
        }
      }
    }
  }

  def shotAIEasy(r : scala.util.Random, ships : List[Ship], shipGrid : Grid): (Int,Ship,Int,Int) ={
    val shotRow = r.nextInt(10)
    val shotCol = r.nextInt(10)
    val result = Player.makeShot(shotRow,shotCol,ships,shipGrid)
    (result._1,result._2,shotRow,shotCol)
  }

  def shotAIMedium(r : scala.util.Random, ships : List[Ship], shipGrid : Grid): (Int,Ship,Int,Int) ={
    val shotRow = r.nextInt(10)
    val shotCol = r.nextInt(10)
    if(shipGrid.gridFilled(shotRow)(shotCol)==1||shipGrid.gridFilled(shotRow)(shotCol)==2){
      shotAIMedium(r,ships,shipGrid)
    }else{
      val result = Player.makeShot(shotRow,shotCol,ships,shipGrid)
      (result._1,result._2,shotRow,shotCol)
    }
  }

  def shotAIHard(r : scala.util.Random, ships : List[Ship], shipGrid : Grid, coordShipTouched : List[Int], dirShoot : String): (Int,Ship,Int,Int) ={
    def shoot(shotRow : Int, shotCol : Int): (Int,Ship,Int,Int) ={
      if(shipGrid.gridFilled(shotRow)(shotCol)==1||shipGrid.gridFilled(shotRow)(shotCol)==2){
        shotAIHard(r,ships,shipGrid,coordShipTouched,dirShoot)
      }else{
        val result = Player.makeShot(shotRow,shotCol,ships,shipGrid)
        (result._1,result._2,shotRow,shotCol)
      }
    }
    if(coordShipTouched.head != -1){
      dirShoot match {
        case "top" => shoot(coordShipTouched(0),coordShipTouched(1)+1)
        case "bottom" => shoot(coordShipTouched(0),coordShipTouched(1)-1)
        case "right" => shoot(coordShipTouched(0)+1,coordShipTouched(1))
        case "left" => shoot(coordShipTouched(0)-1,coordShipTouched(1))
      }
    }else{
      shoot(r.nextInt(10),r.nextInt(10))
    }


  }

  def aIPlay() : Unit = {
    val easy = Player("easy",shipGrid = Grid(ships = List()), shotGrid = Grid(ships = List()), isAI = true)
    val medium = Player("medium",shipGrid = Grid(ships = List()), shotGrid = Grid(ships = List()), isAI = true)
    //val hard = Player("hard",shipGrid = Grid(ships = List()), shotGrid = Grid(ships = List()), isAI = true)
    var games : List[Game] = List(Game(players = List(easy, medium))/*,Game(players = List(easy, hard)),Game(players = List(medium, hard))*/)
    games = games.map(g => {
      Game.play(g,5)
    })
    games.foreach(g => println(g.players(0).name + " : " + g.players(0).score + " || " + g.players(1).name + " : " + g.players(1).score))
  }
}