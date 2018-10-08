package example

import scala.annotation.tailrec


case class Grid(ships: List[Ship], gridFilled : List[List[Int]] = List.fill(10)(List.fill(10)(0))) {

  def updateGrid(codeColor : Int, coordinate : List[Int]): List[List[Int]] ={
    this.gridFilled.updated(coordinate(0),this.gridFilled(coordinate(0)).updated(coordinate(1),codeColor))
  }

  //TD delete the ships arg
  def createShips(listNameShips : List[String], ships : List[Ship]) : List[Ship] = {
    if(listNameShips.isEmpty){
      ships
    }else{
      println("Saisie de la position du bateau : "+listNameShips.head)
      val coorRow = Player.initPlacement(codeInput = "row").toInt -1
      val coorCol = Player.initPlacement(codeInput = "col").toInt -1
      val direction = Player.initPlacement(codeInput = "dir")
      val ship = Ship.checkPlacement(listNameShips.head,coorRow,coorCol,direction,ships)

      if(ship.isEmpty){
        createShips(listNameShips, ships)
      }else{
        createShips(listNameShips.tail, ships :+ ship.get)
      }
    }
  }
}

object Grid{
  def placeShip(ships : List[Ship], gridFilled : List[List[Int]]): List[List[Int]] ={
    def changeOneLine(line : List[Int], cols : List[Int]):List[Int]={
      if(cols.isEmpty){
        line
      }else{
        changeOneLine(line.updated(cols.head,3),cols.tail)
      }
    }
    if(ships.isEmpty){
      gridFilled
    }
    else{
      val ship = ships.head
      val rowCoordinates = if(ship.direction == "h"){List(ship.headSquare(0))}else{List.range(ship.headSquare(0),ship.headSquare(0)+Ship.getSize(ship.typeShip))}
      val colCoordinates = if(ship.direction == "v"){List(ship.headSquare(1))} else{List.range(ship.headSquare(1),ship.headSquare(1)+Ship.getSize(ship.typeShip))}
      var gridUpdated = gridFilled
      rowCoordinates.foreach(row =>{
        gridUpdated = gridUpdated.updated(row,changeOneLine(gridUpdated(row),colCoordinates))
      })
      placeShip(ships.tail,gridUpdated)
    }
  }

  //TD Put a for each
  def printOneLine(line : List[Int]): Unit ={
    if(line.nonEmpty){
      print(line.head + " ")
      printOneLine(line.tail)
    }
  }
  @tailrec
  def printShipGrid(grid : List[List[Int]]): Unit ={
    if(grid.nonEmpty){
      printOneLine(grid.head)
      println()
      printShipGrid(grid.tail)
    }
  }

  @tailrec
  def printShotGrid(grid : List[List[Int]]): Unit ={
    if(grid.nonEmpty){
      printOneLine(grid.head)
      println()
      printShotGrid(grid.tail)
    }
  }
}
