package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps
import Direction.*

class CoordSystemTest extends AnyFunSuite {

  test("EvenHorizontalCoordSystem") {
    assert(EvenHorizontalCoordSystem().isEven)
    assert(EvenHorizontalCoordSystem().isHorizontal)
    assert(List(NorthEast, East, SouthEast, SouthWest, West, NorthWest) == EvenHorizontalCoordSystem().validDirections)
  }

  test("EvenVerticalCoordSystem") {
    assert(EvenVerticalCoordSystem().isEven)
    assert(!EvenVerticalCoordSystem().isHorizontal)
    assert(List(North, NorthEast, SouthEast, South, SouthWest, NorthWest) == EvenVerticalCoordSystem().validDirections)
  }

  test("OddHorizontalCoordSystem") {
    assert(!OddHorizontalCoordSystem().isEven)
    assert(OddHorizontalCoordSystem().isHorizontal)
    assert(List(NorthEast, East, SouthEast, SouthWest, West, NorthWest) == OddHorizontalCoordSystem().validDirections)
  }

  test("OddVerticalCoordSystem") {
    assert(!OddVerticalCoordSystem().isEven)
    assert(!OddVerticalCoordSystem().isHorizontal)
    assert(List(North, NorthEast, SouthEast, South, SouthWest, NorthWest) == OddVerticalCoordSystem().validDirections)
  }  

  test("Conversions") {
    assert(CubeCoord.zero == EvenHorizontalCoordSystem().toCube(Coord.zero))
    assert(Coord.zero == EvenHorizontalCoordSystem().toCoord(CubeCoord.zero))

    assert(CubeCoord.zero == EvenVerticalCoordSystem().toCube(Coord.zero))
    assert(Coord.zero == EvenVerticalCoordSystem().toCoord(CubeCoord.zero))

    assert(CubeCoord.zero == OddHorizontalCoordSystem().toCube(Coord.zero))
    assert(Coord.zero == OddHorizontalCoordSystem().toCoord(CubeCoord.zero))

    assert(CubeCoord.zero == OddVerticalCoordSystem().toCube(Coord.zero))
    assert(Coord.zero == OddVerticalCoordSystem().toCoord(CubeCoord.zero))

    assert(CubeCoord(-1, -2, 3) == EvenHorizontalCoordSystem().toCube(Coord(-2, -2)))
    assert(Coord(-2, -2) == EvenHorizontalCoordSystem().toCoord(CubeCoord(-1, -2, 3)))

    assert(CubeCoord(1, 1, -2) == EvenVerticalCoordSystem().toCube(Coord(1, 2)))
    assert(Coord(1, 2) == EvenVerticalCoordSystem().toCoord(CubeCoord(1, 1, -2)))

    assert(CubeCoord(3, -2, -1) == OddHorizontalCoordSystem().toCube(Coord(2, -2)))
    assert(Coord(2, -2) == OddHorizontalCoordSystem().toCoord(CubeCoord(3, -2, -1)))

    assert(CubeCoord(-2, 2, 0) == OddVerticalCoordSystem().toCube(Coord(-2, 1)))
    assert(Coord(-2, 1) == OddVerticalCoordSystem().toCoord(CubeCoord(-2, 2, 0)))
  }

  test("Neighbours") {
    assert(None == EvenHorizontalCoordSystem().neighborShift(Coord.zero)(North))
    assert(Some(Coord(1,1)) == EvenHorizontalCoordSystem().neighborShift(Coord.zero)(NorthEast))
    assert(Some(Coord(-1,0)) == EvenHorizontalCoordSystem().neighborShift(Coord.zero)(West))

    assert(Some(Coord(1,-1)) == EvenHorizontalCoordSystem().neighborShift(Coord.zero)(SouthEast))
    assert(Some(Coord(0,-1)) == EvenHorizontalCoordSystem().neighborShift(Coord(1,-1))(SouthEast))
    assert(Some(Coord(1,-2)) == EvenHorizontalCoordSystem().neighbor(Coord(1,-1))(SouthEast))

    assert(None == OddHorizontalCoordSystem().neighborShift(Coord.zero)(North))
    assert(Some(Coord(0,1)) == OddHorizontalCoordSystem().neighborShift(Coord.zero)(NorthEast))
    assert(Some(Coord(-1,0)) == OddHorizontalCoordSystem().neighborShift(Coord.zero)(West))
    assert(Some(Coord(0,-1)) == OddHorizontalCoordSystem().neighborShift(Coord.zero)(SouthEast))
    assert(Some(Coord(1,-1)) == OddHorizontalCoordSystem().neighborShift(Coord(0,-1))(SouthEast))
    assert(Some(Coord(1,-2)) == OddHorizontalCoordSystem().neighbor(Coord(0,-1))(SouthEast))

    assert(None == EvenVerticalCoordSystem().neighborShift(Coord.zero)(West))
    assert(Some(Coord(1,1)) == EvenVerticalCoordSystem().neighborShift(Coord.zero)(NorthEast))
    assert(Some(Coord(0,-1)) == EvenVerticalCoordSystem().neighborShift(Coord.zero)(South))
    assert(Some(Coord(1,0)) == EvenVerticalCoordSystem().neighborShift(Coord.zero)(SouthEast))
    assert(Some(Coord(1,-1)) == EvenVerticalCoordSystem().neighborShift(Coord(1,0))(SouthEast))
    assert(Some(Coord(2,-1)) == EvenVerticalCoordSystem().neighbor(Coord(1,0))(SouthEast))

    assert(None ==  OddVerticalCoordSystem().neighborShift(Coord.zero)(West))
    assert(Some(Coord(1,0)) == OddVerticalCoordSystem().neighborShift(Coord.zero)(NorthEast))
    assert(Some(Coord(0,-1)) == OddVerticalCoordSystem().neighborShift(Coord.zero)(South))
    assert(Some(Coord(1,-1)) == OddVerticalCoordSystem().neighborShift(Coord.zero)(SouthEast))
    assert(Some(Coord(1,0)) == OddVerticalCoordSystem().neighborShift(Coord(1,-1))(SouthEast))
    assert(Some(Coord(2,-1)) == OddVerticalCoordSystem().neighbor(Coord(1,-1))(SouthEast))
  }

  test("NeighborList") {
    assert(List(Coord(1,1), Coord(1,0), Coord(1,-1), Coord(0,-1), Coord(-1,0), Coord(0,1)) == EvenHorizontalCoordSystem().neighbors(Coord.zero))
    assert(List(Coord(1,2), Coord(2,1), Coord(1,0), Coord(0,0), Coord(0,1), Coord(0,2)) == EvenHorizontalCoordSystem().neighbors(Coord(1, 1)))
    assert(List(Coord(-1,1), Coord(0,1), Coord(0,0), Coord(-1,-1), Coord(-2,0), Coord(-2,1)) == OddVerticalCoordSystem().neighbors(Coord(-1, 0)))
  }

  test("ClosestFurthest") {
    assert(Option(Coord(0,-2)) == EvenVerticalCoordSystem().closest(Coord.zero)(List(Coord(0,-2), Coord(2,2))))
    assert(Option(Coord(2,2)) == EvenVerticalCoordSystem().furthest(Coord.zero)(List(Coord(0,-2), Coord(2,2))))
    assert(Option(Coord.zero) == EvenVerticalCoordSystem().closest(Coord.zero)(List(Coord.zero, Coord(2,2))))
    assert(Option(Coord(2,2)) == EvenVerticalCoordSystem().furthest(Coord.zero)(List(Coord.zero, Coord(2,2))))
    assert(None == EvenVerticalCoordSystem().closest(Coord.zero)(List()))
    assert(None == EvenVerticalCoordSystem().furthest(Coord.zero)(List()))
  }

  test("Rotate") {
    assert(None == EvenHorizontalCoordSystem().rotate(North)(1))
    assert(Some(NorthWest) == EvenHorizontalCoordSystem().rotateClockwise(West))
    assert(Some(SouthWest) == EvenHorizontalCoordSystem().rotateAnticlockwise(West))
    assert(Some(West) == OddHorizontalCoordSystem().rotate(West)(6))
    assert(Some(NorthWest) == EvenHorizontalCoordSystem().rotate(West)(7))
    assert(Some(SouthEast) == OddVerticalCoordSystem().rotate(North)(2))
    assert(None == EvenVerticalCoordSystem().rotate(East)(-2))
  }

  test("Distance") {
    assert(0 == EvenVerticalCoordSystem().distance(Coord.zero)(Coord.zero))
    assert(3 == OddHorizontalCoordSystem().distance(Coord.zero)(Coord(2,-2)))
    assert(4 == EvenHorizontalCoordSystem().distance(Coord(-1,1))(Coord(1,-2)))
    assert(2 == OddVerticalCoordSystem().distance(Coord(-2,-1))(Coord(-1, 0)))
  }
}
