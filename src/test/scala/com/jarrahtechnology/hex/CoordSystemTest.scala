package com.jarrahtechnology.hex

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert.*

import scala.language.implicitConversions
import scala.language.postfixOps
import Direction.*

class CoordSystemTest extends TestCase {

  @Test def testEvenHorizontalCoordSystem: Unit = {
    assertTrue(EvenHorizontalCoordSystem.isEven)
    assertTrue(EvenHorizontalCoordSystem.isHorizontal)
    assertEquals(List(NorthEast, East, SouthEast, SouthWest, West, NorthWest), EvenHorizontalCoordSystem.validDirections)
  }

  @Test def testEvenVerticalCoordSystem: Unit = {
    assertTrue(EvenVerticalCoordSystem.isEven)
    assertFalse(EvenVerticalCoordSystem.isHorizontal)
    assertEquals(List(North, NorthEast, SouthEast, South, SouthWest, NorthWest), EvenVerticalCoordSystem.validDirections)
  }

  @Test def testOddHorizontalCoordSystem: Unit = {
    assertFalse(OddHorizontalCoordSystem.isEven)
    assertTrue(OddHorizontalCoordSystem.isHorizontal)
    assertEquals(List(NorthEast, East, SouthEast, SouthWest, West, NorthWest), OddHorizontalCoordSystem.validDirections)
  }

  @Test def testOddVerticalCoordSystem: Unit = {
    assertFalse(OddVerticalCoordSystem.isEven)
    assertFalse(OddVerticalCoordSystem.isHorizontal)
    assertEquals(List(North, NorthEast, SouthEast, South, SouthWest, NorthWest), OddVerticalCoordSystem.validDirections)
  }  

  @Test def testConversions: Unit = {
    assertEquals(CubeCoord.zero, EvenHorizontalCoordSystem.toCube(Coord.zero))
    assertEquals(Coord.zero, EvenHorizontalCoordSystem.toCoord(CubeCoord.zero))

    assertEquals(CubeCoord.zero, EvenVerticalCoordSystem.toCube(Coord.zero))
    assertEquals(Coord.zero, EvenVerticalCoordSystem.toCoord(CubeCoord.zero))

    assertEquals(CubeCoord.zero, OddHorizontalCoordSystem.toCube(Coord.zero))
    assertEquals(Coord.zero, OddHorizontalCoordSystem.toCoord(CubeCoord.zero))

    assertEquals(CubeCoord.zero, OddVerticalCoordSystem.toCube(Coord.zero))
    assertEquals(Coord.zero, OddVerticalCoordSystem.toCoord(CubeCoord.zero))

    assertEquals(CubeCoord(-1, -2, 3), EvenHorizontalCoordSystem.toCube(Coord(-2, -2)))
    assertEquals(Coord(-2, -2), EvenHorizontalCoordSystem.toCoord(CubeCoord(-1, -2, 3)))

    assertEquals(CubeCoord(1, 1, -2), EvenVerticalCoordSystem.toCube(Coord(1, 2)))
    assertEquals(Coord(1, 2), EvenVerticalCoordSystem.toCoord(CubeCoord(1, 1, -2)))

    assertEquals(CubeCoord(3, -2, -1), OddHorizontalCoordSystem.toCube(Coord(2, -2)))
    assertEquals(Coord(2, -2), OddHorizontalCoordSystem.toCoord(CubeCoord(3, -2, -1)))

    assertEquals(CubeCoord(-2, 2, 0), OddVerticalCoordSystem.toCube(Coord(-2, 1)))
    assertEquals(Coord(-2, 1), OddVerticalCoordSystem.toCoord(CubeCoord(-2, 2, 0)))
  }

  @Test def testNeighbours: Unit = {
    assertEquals(None, EvenHorizontalCoordSystem.neighborShift(Coord.zero)(North))
    assertEquals(Some(Coord(1,1)), EvenHorizontalCoordSystem.neighborShift(Coord.zero)(NorthEast))
    assertEquals(Some(Coord(-1,0)), EvenHorizontalCoordSystem.neighborShift(Coord.zero)(West))

    assertEquals(Some(Coord(1,-1)), EvenHorizontalCoordSystem.neighborShift(Coord.zero)(SouthEast))
    assertEquals(Some(Coord(0,-1)), EvenHorizontalCoordSystem.neighborShift(Coord(1,-1))(SouthEast))
    assertEquals(Some(Coord(1,-2)), EvenHorizontalCoordSystem.neighbor(Coord(1,-1))(SouthEast))

    assertEquals(None, OddHorizontalCoordSystem.neighborShift(Coord.zero)(North))
    assertEquals(Some(Coord(0,1)), OddHorizontalCoordSystem.neighborShift(Coord.zero)(NorthEast))
    assertEquals(Some(Coord(-1,0)), OddHorizontalCoordSystem.neighborShift(Coord.zero)(West))
    assertEquals(Some(Coord(0,-1)), OddHorizontalCoordSystem.neighborShift(Coord.zero)(SouthEast))
    assertEquals(Some(Coord(1,-1)), OddHorizontalCoordSystem.neighborShift(Coord(0,-1))(SouthEast))
    assertEquals(Some(Coord(1,-2)), OddHorizontalCoordSystem.neighbor(Coord(0,-1))(SouthEast))

    assertEquals(None, EvenVerticalCoordSystem.neighborShift(Coord.zero)(West))
    assertEquals(Some(Coord(1,1)), EvenVerticalCoordSystem.neighborShift(Coord.zero)(NorthEast))
    assertEquals(Some(Coord(0,-1)), EvenVerticalCoordSystem.neighborShift(Coord.zero)(South))
    assertEquals(Some(Coord(1,0)), EvenVerticalCoordSystem.neighborShift(Coord.zero)(SouthEast))
    assertEquals("e-v 2nd row", Some(Coord(1,-1)), EvenVerticalCoordSystem.neighborShift(Coord(1,0))(SouthEast))
    assertEquals(Some(Coord(2,-1)), EvenVerticalCoordSystem.neighbor(Coord(1,0))(SouthEast))

    assertEquals(None, OddVerticalCoordSystem.neighborShift(Coord.zero)(West))
    assertEquals(Some(Coord(1,0)), OddVerticalCoordSystem.neighborShift(Coord.zero)(NorthEast))
    assertEquals(Some(Coord(0,-1)), OddVerticalCoordSystem.neighborShift(Coord.zero)(South))
    assertEquals(Some(Coord(1,-1)), OddVerticalCoordSystem.neighborShift(Coord.zero)(SouthEast))
    assertEquals(Some(Coord(1,0)), OddVerticalCoordSystem.neighborShift(Coord(1,-1))(SouthEast))
    assertEquals(Some(Coord(2,-1)), OddVerticalCoordSystem.neighbor(Coord(1,-1))(SouthEast))
  }

  @Test def testNeighborList: Unit = {
    assertEquals("a", List(Coord(1,1), Coord(1,0), Coord(1,-1), Coord(0,-1), Coord(-1,0), Coord(0,1)), EvenHorizontalCoordSystem.neighbors(Coord.zero))
    assertEquals("b", List(Coord(1,2), Coord(2,1), Coord(1,0), Coord(0,0), Coord(0,1), Coord(0,2)), EvenHorizontalCoordSystem.neighbors(Coord(1, 1)))
    assertEquals("c", List(Coord(-1,1), Coord(0,1), Coord(0,0), Coord(-1,-1), Coord(-2,0), Coord(-2,1)), OddVerticalCoordSystem.neighbors(Coord(-1, 0)))
  }

  @Test def testClosestFurthest: Unit = {
    assertEquals(Option(Coord(0,-2)), EvenVerticalCoordSystem.closest(Coord.zero)(List(Coord(0,-2), Coord(2,2))))
    assertEquals(Option(Coord(2,2)), EvenVerticalCoordSystem.furthest(Coord.zero)(List(Coord(0,-2), Coord(2,2))))
    assertEquals(Option(Coord.zero), EvenVerticalCoordSystem.closest(Coord.zero)(List(Coord.zero, Coord(2,2))))
    assertEquals(Option(Coord(2,2)), EvenVerticalCoordSystem.furthest(Coord.zero)(List(Coord.zero, Coord(2,2))))
    assertEquals(None, EvenVerticalCoordSystem.closest(Coord.zero)(List()))
    assertEquals(None, EvenVerticalCoordSystem.furthest(Coord.zero)(List()))
  }

  @Test def testRotate: Unit = {
    assertEquals(None, EvenHorizontalCoordSystem.rotate(North)(1))
    assertEquals(Some(NorthWest), EvenHorizontalCoordSystem.rotateClockwise(West))
    assertEquals(Some(SouthWest), EvenHorizontalCoordSystem.rotateAnticlockwise(West))
    assertEquals(Some(West), OddHorizontalCoordSystem.rotate(West)(6))
    assertEquals(Some(NorthWest), EvenHorizontalCoordSystem.rotate(West)(7))
    assertEquals(Some(SouthEast), OddVerticalCoordSystem.rotate(North)(2))
    assertEquals(None, EvenVerticalCoordSystem.rotate(East)(-2))
  }

  @Test def testDistance: Unit = {
    assertEquals(0, EvenVerticalCoordSystem.distance(Coord.zero)(Coord.zero))
    assertEquals(3, OddHorizontalCoordSystem.distance(Coord.zero)(Coord(2,-2)))
    assertEquals(4, EvenHorizontalCoordSystem.distance(Coord(-1,1))(Coord(1,-2)))
    assertEquals(2, OddVerticalCoordSystem.distance(Coord(-2,-1))(Coord(-1, 0)))
  }
}
