package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps

class HexCollectionTest extends AnyFunSuite {
  test("HexCollection Constructor") {
    val grid = HexCollection(EvenHorizontalCoordSystem, Map(Coord.zero -> "a", Coord(1,1) -> "b"))
    assert(EvenHorizontalCoordSystem == grid.coords)
    assert(grid.hexAt(Coord.zero) == Some("a"))
    assert(grid.hexAtWithCoord(Coord.zero) == Some(Coord.zero, "a"))
    assert(grid.hexAt(Coord(0, 1)) == None)
  }

  test("HexCollection Iterator") {
    val iter = HexCollection(EvenHorizontalCoordSystem, Map(Coord.zero -> "a", Coord(1,1) -> "b", Coord(0,1) -> "c")).iterator
    assert(iter.next()._2=="a")
    assert(iter.next()._2=="b")
    assert(iter.next()._2=="c")
    assert(!iter.hasNext)
  }

  test("HexCollection IteratorOps") {
    val grid = HexCollection(EvenHorizontalCoordSystem, Map(Coord.zero -> "a", Coord(1,1) -> "b", Coord(0,1) -> "c"))
    assert(grid.size==3)
    val posGrid = grid.filter((c, h) => c.column>0)
    assert(posGrid.size==1)
    assert(grid.size==3)
  }

  test("HexCollection ImmutableSet") {
    val grid = HexCollection(EvenHorizontalCoordSystem, Map(Coord.zero -> "a", Coord(1,1) -> "b"))
    assert(grid.hexAt(Coord(1,1))==Some("b"))
    assert(grid.hexAt(Coord(0,2))==None)
    val grid2 = grid.set(Coord(0,2), "other")
    assert(grid.hexAt(Coord(1,1))==Some("b"))
    assert(grid.hexAt(Coord(0,2))==None)
    assert(grid2.hexAt(Coord(0,2))==Some("other"))
    assert(grid2.hexAt(Coord(1,1))==Some("b"))
  }

  test("HexCollection ImmutableSetMult") {
    val grid = HexCollection(EvenHorizontalCoordSystem, Map(Coord.zero -> "a", Coord(1,1) -> "b"))
    assert(grid.hexAt(Coord(1,1))==Some("b"))
    assert(grid.hexAt(Coord(0,2))==None)
    val grid2 = grid.set(Map(Coord(0,2) -> "other", Coord(1,2) -> "other2"))
    assert(grid.hexAt(Coord(1,1))==Some("b"))
    assert(grid.hexAt(Coord(0,2))==None)
    assert(grid2.hexAt(Coord(0,2))==Some("other"))
    assert(grid2.hexAt(Coord(1,2))==Some("other2"))
    assert(grid2.hexAt(Coord(1,1))==Some("b"))
  }
}
