package example

import scala.annotation.tailrec

case class Ship(typeShip: String, headSquare: List[Int], direction: String, lifePoint : Int) {
}

object Ship{
  def getSize(typeShip : String): Int = {
    typeShip match {
      case "Carrier" => 5
      case "Battleship" => 4
      case "Cruiser" => 3
      case "Submarine" => 3
      case "Destroyer" => 2
    }
  }

  @tailrec
  //TD check the minus 1
  def checkPlacement(typeShip : String,rowShip : Int, colShip : Int, dirShip : String,ships : List[Ship]) : Option[Ship] = {
    def checkShipAligned(compareShipCoord : List[Int]): Boolean ={
      val compareShip = ships.head
      if(rowShip==compareShip.headSquare(compareShipCoord.head)){
        if(List.range(colShip,colShip+getSize(typeShip)).intersect(List.range(compareShip.headSquare(compareShipCoord(1)),compareShip.headSquare(compareShipCoord(1))+getSize(compareShip.typeShip))).isEmpty){
          true
        }else{
          println("<!> Ships overlap, enter another value")
          false
        }
      }else{
        true
      }
    }
    //TD put sub-functions
    //****************************************************
    if((dirShip =="h"&&colShip+getSize(typeShip)>9)||(dirShip =="v"&&rowShip+getSize(typeShip)>9)){
      println("<!> The entire ship is out of bound.")
      None
    }else{
      if(ships.isEmpty){
        Some(Ship(typeShip,List(rowShip,colShip), dirShip,Ship.getSize(typeShip)))
      }else{
        val compareShip = ships.head
        dirShip match{
          case "h" => {
            if(compareShip.direction == "h"){
              if(checkShipAligned(List(0,1))){checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)}else{None}
            }else{
              val listRowTmp = List.range(compareShip.headSquare.head,compareShip.headSquare.head + getSize(compareShip.typeShip))
              if(listRowTmp.contains(rowShip)){
                //val listColTmp = colShip until colShip + getSize(typeShip) //-1
                val listColTmp = List.range(colShip,colShip + getSize(typeShip))
                if(listColTmp.contains(compareShip.headSquare(1))){
                  println("<!> Ships overlap, enter another value.")
                  None
                }else{
                  checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
                }
              }else{
                checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
              }
            }
          }
          case "v" => {
            if(compareShip.direction == "v"){
              if(checkShipAligned(List(1,0))){checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)}else{None}
            }else{
             // val listRowTmp = rowShip until rowShip + getSize(typeShip) //-1
              val listRowTmp = List.range(rowShip,rowShip + getSize(typeShip))
              if(listRowTmp.contains(compareShip.headSquare.head)){
                val listColTmp = List.range(compareShip.headSquare(1),compareShip.headSquare(1) + getSize(compareShip.typeShip))
                if(listColTmp.contains(colShip)){
                  println("<!> Ships overlap, enter another value.")
                  None
                }else{
                  checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
                }
              }else{
                checkPlacement(typeShip,rowShip,colShip,dirShip,ships.tail)
              }
            }
          }
        }
      }
    }
  }
}