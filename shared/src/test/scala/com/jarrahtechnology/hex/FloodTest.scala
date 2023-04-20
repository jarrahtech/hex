package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps

class FloodTest extends AnyFunSuite {

  test("Flood dist<0") {
    assertThrows[IllegalArgumentException] {
      Flood.fill(RectangularHexGrid.immutable(EvenVerticalCoordSystem(), 5, 5, (c,r) => s"(${c},${r})"))(Coord.zero, -1)
    }
  }

  test("Flood dist=0") {
    val grid = RectangularHexGrid.immutable(CoordSystem.evenVertical, 5, 5, (c,r) => s"(${c},${r})")
    assert(Flood.fill(grid)(Coord.zero, 0)==ImmutableSparseHexGrid(CoordSystem.evenVertical, Map(Coord.zero -> "(0,0)")))
  }

  test("Flood dist=2") {
    val grid = RectangularHexGrid.immutable(CoordSystem.evenVertical, 5, 5, (c,r) => s"(${c},${r})")
    assert(Flood.fill(grid)(Coord(1, 1), 2)==ImmutableSparseHexGrid(CoordSystem.evenVertical, Map(
                      Coord(2, 2) -> "(2,2)", Coord(1, 0) -> "(1,0)", Coord(3, 1) -> "(3,1)", 
                      Coord(1, 1) -> "(1,1)", Coord(0, 2) -> "(0,2)", Coord(0, 1) -> "(0,1)", 
                      Coord(0, 0) -> "(0,0)", Coord(2, 0) -> "(2,0)", Coord(3, 2) -> "(3,2)", 
                      Coord(2, 1) -> "(2,1)", Coord(3, 0) -> "(3,0)", Coord(1, 2) -> "(1,2)", Coord(1, 3) -> "(1,3)")
    ))
  }
  
}
