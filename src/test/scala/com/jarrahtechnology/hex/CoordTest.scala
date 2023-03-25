package com.jarrahtechnology.hex

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert.*

import scala.language.implicitConversions
import scala.language.postfixOps

class CoordTest extends TestCase {

  @Test def testCoord: Unit = {
    assertEquals(3, Coord(3, 2).column)
    assertEquals(2, Coord(3, 2).row)
  }

  @Test def testCoordAdd: Unit = {
    assertEquals(Coord.zero, Coord.zero+Coord.zero)
    assertEquals(Coord.down, Coord.zero+Coord.down)
    assertEquals(Coord(12, -56), Coord(5, 2)+Coord(7, -58))
  }

  @Test def testCoordMinus: Unit = {
    assertEquals(Coord.zero, Coord.zero-Coord.zero)
    assertEquals(Coord.up, Coord.zero-Coord.down)
    assertEquals(Coord(-2, 60), Coord(5, 2)-Coord(7, -58))
  }

  @Test def testCoordNegate: Unit = {
    assertEquals(Coord.zero, -Coord.zero)
    assertEquals(Coord.down, -Coord.up)
    assertEquals(Coord.up, -Coord.down)
  }

  @Test def testCubeCoordAdd: Unit = {
    assertEquals(CubeCoord.zero, CubeCoord.zero+CubeCoord.zero)
    assertEquals(CubeCoord(1, 1, 1), CubeCoord.zero+CubeCoord(1, 1, 1))
    assertEquals(CubeCoord(12, -56, 76), CubeCoord(5, 2, 73)+CubeCoord(7, -58, 3))
  }

  @Test def testCubeCoordMinus: Unit = {
    assertEquals(CubeCoord.zero, CubeCoord.zero-CubeCoord.zero)
    assertEquals(CubeCoord(1, 1, 1), CubeCoord.zero-CubeCoord(-1, -1, -1))
    assertEquals(CubeCoord(-2, 60, 70), CubeCoord(5, 2, 73)-CubeCoord(7, -58, 3))
  }

  @Test def testCubeCoordNegate: Unit = {
    assertEquals(CubeCoord.zero, -CubeCoord.zero)
    assertEquals(CubeCoord(-3, -4, 5), -CubeCoord(3, 4, -5))
    assertEquals(CubeCoord(3, 4, -5), -CubeCoord(-3, -4, 5))
  }

  @Test def testCubeCoordDistance: Unit = {
    assertEquals(0, CubeCoord.zero.distance(CubeCoord.zero))
    assertEquals(1, CubeCoord(0, -1, 1).distance(CubeCoord.zero))
    assertEquals(7, CubeCoord(-1, -3, 4).distance(CubeCoord(3, 0, -3)))
  }

  @Test def testCubeCoordRound: Unit = {
    assertEquals(CubeCoord.zero, CubeCoord.round(0, 0, 0))
    assertEquals(CubeCoord(1, 2, -3), CubeCoord.round(1.3, 2, 3.7))
    assertEquals(CubeCoord(1, 2, -3), CubeCoord.round(1.3, 2, -3.7))
  }

}
