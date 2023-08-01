package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps

// TODO: more tests
class AStarTest extends AnyFunSuite {

  test("Sparse AStar") {
    val grid = ImmutableSparseHexGrid(OddHorizontalCoordSystem(), Map(Coord.zero->"a",Coord(0,2)->"e",Coord(1,0)->"b",Coord(1,1)->"c",Coord(1,2)->"c",Coord(1,3)->"x")) 
    // shortest through (0,1), but not defined
    
    assert(List(Coord.zero,Coord(0,1),Coord(0,2))==AStar.sparsePath(grid, Coord.zero, Coord(0,2)))
    assert(List(Coord.zero,Coord(1,0),Coord(1,1),Coord(1,2),Coord(0,2))==AStar.path(grid, Coord.zero, Coord(0,2)))
  }
}