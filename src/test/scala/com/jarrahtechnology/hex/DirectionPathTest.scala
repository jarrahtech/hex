package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps

import Direction._
import CoordSystem._
import scala.collection.mutable.ArrayBuffer

class DirectionPathTest extends AnyFunSuite {
  
  test("DirectionPath empty") {
    assertThrows[IllegalArgumentException] {
      DirectionPath(List.empty)
    }
  }

  test("DirectionPath constructor") {
    val path = DirectionPath(List(Some(North), None, Some(NorthEast)))
    assert(path.length==3)
    val iter = path.iterator
    assert(iter.next()==Some(North))
    assert(iter.next()==None)
    assert(iter.next()==Some(NorthEast))
    assert(!iter.hasNext)
  }

  test("DirectionPath toCoords") {
    val path = DirectionPath(List(Some(East), None, Some(NorthEast)))
    assert(path.toCoords(evenHorizontal)(Coord.zero)==Some(List(Coord(1,0),Coord(1,0),Coord(2,1))))
    assert(path.toCoords(evenHorizontal)(Coord(1,1))==Some(List(Coord(2,1),Coord(2,1),Coord(2,2))))
    assert(path.toCoords(evenVertical)(Coord(1,1))==None)
    assert(DirectionPath.none.toCoords(evenVertical)(Coord(2,1))==Some(List(Coord(2,1))))
  }

  test("DirectionPath toCoords negative") {
    val path = DirectionPath(List(Some(South), None, Some(SouthWest)))
    assert(path.toCoords(evenVertical)(Coord(0,2))==Some(List(Coord(0, 1), Coord(0, 1), Coord(-1, 1))))
  }

  test("DirectionPath toPath in bounds") {
    val path = DirectionPath(List(Some(East), None, Some(NorthEast)))
    val grid = RectangularHexGrid.immutable(evenHorizontal, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.toPath(grid)(Coord.zero)==Some(List((Coord(1,0),"(1,0)"),(Coord(1,0),"(1,0)"),(Coord(2,1),"(2,1)"))))
  }

  test("DirectionPath toPath out bounds") {
    val path = DirectionPath(List(Some(South), None, Some(SouthWest)))
    val grid = RectangularHexGrid.immutable(evenVertical, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.toPath(grid)(Coord(0,2))==None)
  }

  test("DirectionPath toPath invalid") {
    val path = DirectionPath(List(Some(East), None, Some(NorthEast), Some(SouthWest)))
    val grid = RectangularHexGrid.immutable(evenVertical, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.toPath(grid)(Coord.zero)==None)
  }

  test("DirectionPath toPathEnd in bounds") {
    val path = DirectionPath(List(Some(East), None, Some(NorthEast)))
    val grid = RectangularHexGrid.immutable(evenHorizontal, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.toPathEnd(grid)(Coord.zero)==Some((Coord(2,1),"(2,1)")))
  }

  test("DirectionPath toPathEnd out bounds") {
    val path = DirectionPath(List(Some(South), None, Some(SouthWest), Some(NorthEast)))
    val grid = RectangularHexGrid.immutable(evenVertical, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.toPathEnd(grid)(Coord(0,2))==None)
  }

  test("DirectionPath toPathEnd invalid") {
    val path = DirectionPath(List(Some(East), None, Some(NorthEast), Some(SouthWest)))
    val grid = RectangularHexGrid.immutable(evenVertical, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.toPath(grid)(Coord.zero)==None)
  }

  test("DirectionPath skipToPathEnd in bounds") {
    val path = DirectionPath(List(Some(East), None, Some(NorthEast)))
    val grid = RectangularHexGrid.immutable(evenHorizontal, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.skipToPathEnd(grid)(Coord.zero)==Some((Coord(2,1),"(2,1)")))
  }

  test("DirectionPath skipToPathEnd out bounds") {
    val path = DirectionPath(List(Some(South), None, Some(SouthWest), Some(NorthEast)))
    val grid = RectangularHexGrid.immutable(evenVertical, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.skipToPathEnd(grid)(Coord(0,2))==Some((Coord(0, 1), "(0,1)")))
    Coord(0, 1)
  }

  test("DirectionPath skipToPathEnd invalid") {
    val path = DirectionPath(List(Some(East), None, Some(NorthEast), Some(SouthWest)))
    val grid = RectangularHexGrid.immutable(evenVertical, 4, 4, (c,r) => s"(${c},${r})")
    assert(path.toPath(grid)(Coord.zero)==None)
  }

  test("DirectionPath applyTo in bounds") {
    val result = ArrayBuffer[(Int, Coord, Option[String])]()
    val path = DirectionPath(List(Some(East), None, Some(NorthEast)))
    val grid = RectangularHexGrid.immutable(evenHorizontal, 4, 4, (c,r) => s"(${c},${r})")
    path.applyTo(grid)(Coord.zero, (a, b, c) => result.addOne((a, b, c)))
    assert(result == ArrayBuffer((0,Coord(1,0),Some("(1,0)")),(1,Coord(1,0),Some("(1,0)")),(2,Coord(2,1),Some("(2,1)"))))
  }

  test("DirectionPath applyTo out bounds") {
    val result = ArrayBuffer[(Int, Coord, Option[String])]()
    val path = DirectionPath(List(Some(South), None, Some(SouthWest)))
    val grid = RectangularHexGrid.immutable(evenVertical, 4, 4, (c,r) => s"(${c},${r})")
    path.applyTo(grid)(Coord(0,2), (a, b, c) => result.addOne((a, b, c)))
    assert(result == ArrayBuffer((0,Coord(0, 1),Some("(0,1)")),(1,Coord(0, 1),Some("(0,1)")),(2,Coord(-1, 1),None)))
  }

  test("DirectionPath applyTo invalid") {
    val result = ArrayBuffer[(Int, Coord, Option[String])]()
    val path = DirectionPath(List(Some(East), None, Some(NorthEast), Some(SouthWest)))
    val grid = RectangularHexGrid.immutable(evenVertical, 4, 4, (c,r) => s"(${c},${r})")
    path.applyTo(grid)(Coord(0,2), (a, b, c) => result.addOne((a, b, c)))
    assert(result.isEmpty)
  }
}
