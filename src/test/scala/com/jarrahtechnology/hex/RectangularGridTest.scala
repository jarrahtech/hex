package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.collection.mutable.ArraySeq

class RectangularGridTest extends AnyFunSuite {

  test("MutableConstructor") {
    val grid = MutableRectangularHexGrid(EvenHorizontalCoordSystem(), (0,1), (0,1), ArraySeq.tabulate[String](2, 2)((c,r) => s"(${c},${r})"))
    assert(2 == grid.numRows)
    assert(2 == grid.numColumns)
    assert(4 == grid.size)
  }

  test("ImmutableConstructor") {
    val grid = ImmutableRectangularHexGrid(OddHorizontalCoordSystem(), (-1,1), (-1,1), List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    assert(3 == grid.numRows)
    assert(3 == grid.numColumns)
    assert(9 == grid.size)
  }

  test("MutableGeneratorConstructor") {
    val grid = RectangularHexGrid.mutable(EvenHorizontalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})")
    assert(5 == grid.numRows)
    assert(4 == grid.numColumns)
    assert(20 == grid.size)

    val grid2 = RectangularHexGrid.mutable(EvenHorizontalCoordSystem(), 4, 5, (c,r) => s"(${c},${r})")
    assert(6 == grid2.numRows)
    assert(5 == grid2.numColumns)
    assert(30 == grid2.size)
  }

  test("ImmutableGeneratorConstructor") {
    val grid = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-27,1), (1,96), (c,r) => s"(${c},${r})")
    assert(96 == grid.numRows)
    assert(29 == grid.numColumns)
    assert(2784 == grid.size)

    val grid2 = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), 34, 56, (c,r) => s"(${c},${r})")
    assert(57 == grid2.numRows)
    assert(35 == grid2.numColumns)
    assert(1995 == grid2.size)
  }

  test("ImmutableGeneratorNegative") {
    assertThrows[IllegalArgumentException] {
      RectangularHexGrid.immutable(EvenVerticalCoordSystem(), -12, 56, (c,r) => s"(${c},${r})")
    }
  }

  test("MutableSet") {
    val grid = RectangularHexGrid.mutable(EvenHorizontalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.hexAt(Coord(1,2))==Some("(1,2)"))
    assert(grid.hexAt(Coord(0,2))==Some("(0,2)"))
    grid.set(Map(Coord(1,2) -> "other"))
    assert(grid.hexAt(Coord(1,2))==Some("other"))
    assert(grid.hexAt(Coord(0,2))==Some("(0,2)"))
  }

  test("ImmutableSet") {
    val grid = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.hexAt(Coord(1,2))==Some("(1,2)"))
    assert(grid.hexAt(Coord(0,2))==Some("(0,2)"))
    val grid2 = grid.set(Coord(1,2), "other")
    assert(grid.hexAt(Coord(1,2))==Some("(1,2)"))
    assert(grid.hexAt(Coord(0,2))==Some("(0,2)"))
    assert(grid2.hexAt(Coord(1,2))==Some("other"))
    assert(grid2.hexAt(Coord(0,2))==Some("(0,2)"))
  }

  test("ImmutableSetMult") {
    val grid = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.hexAt(Coord(1,2))==Some("(1,2)"))
    assert(grid.hexAt(Coord(0,2))==Some("(0,2)"))
    val grid2 = grid.set(Map(Coord(1,2) -> "other"))
    assert(grid.hexAt(Coord(1,2))==Some("(1,2)"))
    assert(grid.hexAt(Coord(0,2))==Some("(0,2)"))
    assert(grid2.hexAt(Coord(1,2))==Some("other"))
    assert(grid2.hexAt(Coord(0,2))==Some("(0,2)"))
  }

  test("Asserts") {
    assertThrows[IllegalArgumentException] {
      RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,-3), (0,4), (c,r) => s"(${c},${r})")
    }
    assertThrows[IllegalArgumentException] {
      RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,-4), (c,r) => s"(${c},${r})")
    }

    assertThrows[IllegalArgumentException] {
      ImmutableRectangularHexGrid(OddHorizontalCoordSystem(), (-1,1), (-1,1), List(List(1, 2, 3), List(4, 5, 6), List(7, 8)))
    }

    assertThrows[IllegalArgumentException] {
      ImmutableRectangularHexGrid(OddHorizontalCoordSystem(), (-1,1), (-1,1), List(List(1, 2, 3), List(4, 5, 6)))
    }
  }

  test("Valid") {
    val grid = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.inBounds(Coord(1, 2)))
    assert(!grid.inBounds(Coord(1, -2)))
  }

  test("Iterator col") {
    val iter = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})").colByRowIterator()
    assert(iter.next()._2=="(-2,0)")
    assert(iter.next()._2=="(-2,1)")
    assert(iter.next()._2=="(-2,2)")
    assert(iter.next()._2=="(-2,3)")
    assert(iter.next()._2=="(-2,4)")
    assert(iter.next()._2=="(-1,0)")
    assert(iter.next()._2=="(-1,1)")
    assert(iter.next()._2=="(-1,2)")
    assert(iter.next()._2=="(-1,3)")
    assert(iter.next()._2=="(-1,4)")
    assert(iter.next()._2=="(0,0)")
    assert(iter.next()._2=="(0,1)")
    assert(iter.next()._2=="(0,2)")
    assert(iter.next()._2=="(0,3)")
    assert(iter.next()._2=="(0,4)")
    assert(iter.next()._2=="(1,0)")
    assert(iter.next()._2=="(1,1)")
    assert(iter.next()._2=="(1,2)")
    assert(iter.next()._2=="(1,3)")
    assert(iter.next()._2=="(1,4)")
    assert(!iter.hasNext)
  }

  test("Iterator row") {
    val iter = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})").rowByColIterator()
    assert(iter.next()._2=="(-2,0)")
    assert(iter.next()._2=="(-1,0)")
    assert(iter.next()._2=="(0,0)")
    assert(iter.next()._2=="(1,0)")
    assert(iter.next()._2=="(-2,1)")
    assert(iter.next()._2=="(-1,1)")
  }

  test("IteratorOps") {
    val grid = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.size==20)
    val posGrid = grid.filter((c, h) => c.column>=0)
    assert(posGrid.size==10)
  }

  test("Distance") {
    assert(0 == RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})").distance(Coord.zero)(Coord.zero))
    assert(3 == RectangularHexGrid.immutable(OddHorizontalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})").distance(Coord.zero)(Coord(2,-2)))
    assert(4 == RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})").distance(Coord(-1,1))(Coord(1,-2)))
    assert(2 == RectangularHexGrid.immutable(OddVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})").distance(Coord(-2,-1))(Coord(-1, 0)))
    assert(RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,2), (0,4), (c,r) => s"(${c},${r})").distance(Coord(1, 1))(Coord(1, 3))==2)
  }

  test("Neighbors with coord") {
    val grid = RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,2), (0,4), (c,r) => s"(${c},${r})")
    assert(List((Coord(1,2), "(1,2)"), (Coord(2,1), "(2,1)"), (Coord(1,0), "(1,0)"), (Coord(0,0), "(0,0)"), (Coord(0,1), "(0,1)"), (Coord(0,2), "(0,2)")) == grid.neighborsWithCoord(Coord(1, 1)))
  }

  test("Neighbors") {
    val grid = RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})")
    assert(List("(1,2)", "(1,0)", "(0,0)", "(0,1)", "(0,2)") == grid.neighbors(Coord(1, 1)))
  }

  test("Range") {
    val grid = RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,2), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.rangeWithCoord(Coord(1, 1), 1)==Vector((Coord(0, 1), "(0,1)"), (Coord(0, 2), "(0,2)"), (Coord(0, 0), "(0,0)"), (Coord(1, 1), "(1,1)"), (Coord(1, 2), "(1,2)"), (Coord(1, 0), "(1,0)"), (Coord(2, 1), "(2,1)")))
  }

  test("Closest") {
    val grid = RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,2), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.closest(Coord(1, 1))(List(Coord(1, 3), Coord(-2, -2), Coord(0, 2)))==Some("(0,2)"))
  }

  test("Closest same") {
    val grid = RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,2), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.closest(Coord(1, 1))(List(Coord(1, 3), Coord(-2, -2), Coord(1, 1), Coord(0, 2)))==Some("(1,1)"))
  }

  test("Closest none") {
    val grid = RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,2), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.closest(Coord(1, 1))(List())==None)
  }

  test("Furthest") {
    val grid = RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), (-2,2), (0,4), (c,r) => s"(${c},${r})")
    assert(grid.furthestWithCoord(Coord(1, 1))(List(Coord(1, 3), Coord(-2, -2), Coord(0, 2)))==None)
    assert(grid.furthestWithCoord(Coord(1, 1))(List(Coord(1, 3), Coord(-2, 4), Coord(0, 2)))==Some((Coord(-2,4),"(-2,4)")))
  }
}
