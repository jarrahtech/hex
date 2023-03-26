package com.jarrahtechnology.hex

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert.*

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.collection.mutable.ArraySeq

class RectangularGridTest extends TestCase {

  @Test def testMutableConstructor: Unit = {
    val grid = MutableRectangularHexGrid(EvenHorizontalCoordSystem, (0,1), (0,1), ArraySeq.tabulate[String](2, 2)((c,r) => s"(${c},${r})"))
    assertEquals(2, grid.numRows)
    assertEquals(2, grid.numColumns)
    assertEquals(4, grid.size)
  }

  @Test def testImmutableConstructor: Unit = {
    val grid = ImmutableRectangularHexGrid(OddHorizontalCoordSystem, (-1,1), (-1,1), List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    assertEquals(3, grid.numRows)
    assertEquals(3, grid.numColumns)
    assertEquals(9, grid.size)
  }

  @Test def testMutableGeneratorConstructor: Unit = {
    val grid = RectangularHexGrid.mutable(EvenHorizontalCoordSystem, (-2,1), (0,4), (c,r) => s"(${c},${r})")
    assertEquals(5, grid.numRows)
    assertEquals(4, grid.numColumns)
    assertEquals(20, grid.size)

    val grid2 = RectangularHexGrid.mutable(EvenHorizontalCoordSystem, 4, 5, (c,r) => s"(${c},${r})")
    assertEquals(6, grid2.numRows)
    assertEquals(5, grid2.numColumns)
    assertEquals(30, grid2.size)
  }

  @Test def testImmutableGeneratorConstructor: Unit = {
    val grid = RectangularHexGrid.immutable(EvenVerticalCoordSystem, (-27,1), (1,96), (c,r) => s"(${c},${r})")
    assertEquals(96, grid.numRows)
    assertEquals(29, grid.numColumns)
    assertEquals(2784, grid.size)

    val grid2 = RectangularHexGrid.immutable(EvenVerticalCoordSystem, 34, 56, (c,r) => s"(${c},${r})")
    assertEquals(57, grid2.numRows)
    assertEquals(35, grid2.numColumns)
    assertEquals(1995, grid2.size)
  }

  @Test(expected=classOf[IllegalArgumentException]) 
  def testImmutableGeneratorNegative: Unit = {
    //RectangularHexGrid.immutable(EvenVerticalCoordSystem, -12, 56, (c,r) => s"(${c},${r})")
  }

  @Test def testMutableSet: Unit = {
  }

  @Test def testImmutableSet: Unit = {
  }

  @Test def testWidthHeight: Unit = {
  }

  @Test def testAsserts: Unit = {
  }

  @Test def testValid: Unit = {
  }

  @Test def testHexAt: Unit = {
  }

  @Test def testIterator: Unit = {
  }

  @Test def testIteratorOps: Unit = {
  }

  @Test def testAltIterator: Unit = {
  }

  @Test def testDistance: Unit = {
  }

  @Test def testNeighbors: Unit = {
  }

  @Test def testRange: Unit = {
  }

  @Test def testClosest: Unit = {
  }

  @Test def testFurthest: Unit = {
  }
}
