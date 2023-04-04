package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps

class CoordTest extends AnyFunSuite {

  test("Coord") {
    assert(3==Coord(3, 2).column)
    assert(2==Coord(3, 2).row)
  }

  test("CoordAdd") {
    assert(Coord.zero==Coord.zero+Coord.zero)
    assert(Coord.down==Coord.zero+Coord.down)
    assert(Coord(12, -56)==Coord(5, 2)+Coord(7, -58))
  }

  test("CoordMinus") {
    assert(Coord.zero==Coord.zero-Coord.zero)
    assert(Coord.up==Coord.zero-Coord.down)
    assert(Coord(-2, 60)==Coord(5, 2)-Coord(7, -58))
  }

  test("CoordNegate") {
    assert(Coord.zero== -Coord.zero)
    assert(Coord.down== -Coord.up)
    assert(Coord.up== -Coord.down)
  }

  test("CubeCoordAdd") {
    assert(CubeCoord.zero==CubeCoord.zero+CubeCoord.zero)
    assert(CubeCoord(1, 1, 1)==CubeCoord.zero+CubeCoord(1, 1, 1))
    assert(CubeCoord(12, -56, 76)==CubeCoord(5, 2, 73)+CubeCoord(7, -58, 3))
  }

  test("CubeCoordMinus") {
    assert(CubeCoord.zero==CubeCoord.zero-CubeCoord.zero)
    assert(CubeCoord(1, 1, 1)==CubeCoord.zero-CubeCoord(-1, -1, -1))
    assert(CubeCoord(-2, 60, 70)==CubeCoord(5, 2, 73)-CubeCoord(7, -58, 3))
  }

  test("CubeCoordNegate") {
    assert(CubeCoord.zero== -CubeCoord.zero)
    assert(CubeCoord(-3, -4, 5)== -CubeCoord(3, 4, -5))
    assert(CubeCoord(3, 4, -5)== -CubeCoord(-3, -4, 5))
  }

  test("CubeCoordDistance") {
    assert(0==CubeCoord.zero.distance(CubeCoord.zero))
    assert(1==CubeCoord(0, -1, 1).distance(CubeCoord.zero))
    assert(7==CubeCoord(-1, -3, 4).distance(CubeCoord(3, 0, -3)))
  }

  test("CubeCoordRound") {
    assert(CubeCoord.zero==CubeCoord.round(0, 0, 0))
    assert(CubeCoord(1, 2, -3)==CubeCoord.round(1.3, 2, 3.7))
    assert(CubeCoord(1, 2, -3)==CubeCoord.round(1.3, 2, -3.7))
  }

}
