package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps

class CompositeHexGridTest extends AnyFunSuite {
  
  test("HexGridOverlay Constructor") {
    val grid = CompositeHexGrid.overlay(List(RectangularHexGrid.immutable(EvenVerticalCoordSystem(), (-2,1), (0,4), (c,r) => s"(${c},${r})")))
    assert(grid.hexAt(Coord.zero)==Some("(0,0)"))
    assert(grid.hexAt(Coord(21, 1))==None)
  }

  test("HexGridOverlay Constructor underlying exists") {
    assertThrows[IllegalArgumentException] {
      CompositeHexGrid.overlay[Int, EvenVerticalCoordSystem](List())
    }
  }

  test("HexGridOverlay Iterator") {
    val iter = CompositeHexGrid.overlay(List(RectangularHexGrid.immutable(EvenVerticalCoordSystem(), 1, 1, (c,r) => s"(${c},${r})"))).iterator
    assert(iter.next()._2=="(0,0)")
    assert(iter.next()._2=="(0,1)")
    assert(iter.next()._2=="(1,0)")
    assert(iter.next()._2=="(1,1)")
    assert(!iter.hasNext)
  }

  test("HexGridOverlay IteratorOps") {
    val grid =CompositeHexGrid.overlay(List(RectangularHexGrid.immutable(EvenVerticalCoordSystem(), 1, 1, (c,r) => s"(${c},${r})")))
    assert(grid.size==4)
    val posGrid = grid.filter((c, h) => c.column>0)
    assert(posGrid.size==2)
    assert(grid.size==4)
  }

  test("HexGridOverlay allHexAt") {
    val grid =CompositeHexGrid.overlay(List(RectangularHexGrid.immutable(EvenVerticalCoordSystem(), 1, 1, (c,r) => s"(${c},${r})")))
    grid.set(Coord(1, 1), "b")
    assert(grid.allHexAt(Coord(1, 1))==List("b","(1,1)"))
  }

  test("HexGridOverlay MutableSet") {
    val underlying = RectangularHexGrid.immutable(EvenVerticalCoordSystem(), 1, 1, (c,r) => s"(${c},${r})")
    val grid =CompositeHexGrid.overlay(List(underlying))
    assert(grid.hexAt(Coord(1, 1))==Some("(1,1)"))
    grid.set(Coord(1, 1), "b")
    assert(grid.hexAt(Coord(1, 1))==Some("b"))
    assert(underlying.hexAt(Coord(1, 1))==Some("(1,1)"))
  }

  test("HexGridOverlay MutableSetMult") {
    val underlying = RectangularHexGrid.immutable(CoordSystem.evenVertical, 1, 1, (c,r) => s"(${c},${r})")
    val grid =CompositeHexGrid.overlay(List(underlying))
    assert(grid.hexAt(Coord(1, 1))==Some("(1,1)"))
    assert(grid.hexAt(Coord(21, 1))==None)
    grid.set(Map(Coord(1, 1) ->  "b", Coord(21, 1) ->  "c"))
    assert(grid.hexAt(Coord(1, 1))==Some("b"))
    assert(underlying.hexAt(Coord(1, 1))==Some("(1,1)"))
    assert(grid.hexAt(Coord(21, 1))==Some("c"))
    assert(underlying.hexAt(Coord(21, 1))==None)
  }

  test("CompositeHexGrids requirements") {
    assertThrows[IllegalArgumentException] {
      CompositeHexGrids(List(RectangularHexGrid.immutable(EvenVerticalCoordSystem(), 4, 4, (c,r) => s"(${c},${r})")))
    }

    // the below should not compile
    //CompositeHexGrids(List(RectangularHexGrid.immutable(EvenVerticalCoordSystem(), 4, 4, (c,r) => "x"), RectangularHexGrid.immutable(EvenHorizontalCoordSystem(), 4, 4, (c,r) => "x")))
  }

  test("CompositeHexGrids constructor") {
    val grid = CompositeHexGrids(List(ImmutableSparseHexGrid(CoordSystem.evenHorizontal, Map(Coord.zero -> "a", Coord(1,1) -> "b")),
                            ImmutableSparseHexGrid(CoordSystem.evenHorizontal, Map(Coord.zero -> "1", Coord(2,20) -> "2")),
                            RectangularHexGrid.immutable(CoordSystem.evenHorizontal, 4, 4, (c,r) => s"(${c},${r})")))
    assert(grid.size==26)
    assert(grid.hexAt(Coord.zero)==Some("a"))
    assert(grid.allHexAt(Coord.zero)==List("a", "1", "(0,0)"))
    assert(grid.hexAt(Coord(2,2))==Some("(2,2)"))
    assert(grid.hexAt(Coord(2, 20))==Some("2"))
    assert(grid.hexAt(Coord(1, 20))==None)
  }
}
