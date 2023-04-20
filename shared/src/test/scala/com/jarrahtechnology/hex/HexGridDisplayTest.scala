package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps

import com.jarrahtechnology.util.Vector2

class HexGridDisplayTest extends AnyFunSuite {

  test("fromPixel") {
    val grid = RectangularHexGrid.mutable(EvenHorizontalCoordSystem(), 4, 5, (c,r) => s"(${c},${r})")
    val display = HexGridDisplay(grid, 5)
    assert(display.fromPixel(Vector2.zero) == Coord.zero)
    assert(display.fromPixel(Vector2(-2, -2)) == Coord.zero)
    assert(display.fromPixel(Vector2(-4, -4)) == Coord(0,-1))
    assert(display.fromPixel(Vector2(20, 20)) == Coord(3,3))
  }

  test("hexFromPixel") {
    val grid = RectangularHexGrid.mutable(EvenHorizontalCoordSystem(), 4, 5, (c,r) => s"(${c},${r})")
    val display = HexGridDisplay(grid, 5)
    assert(display.hexFromPixel(Vector2.zero) == Some("(0,0)"))
    assert(display.hexFromPixel(Vector2(-4, -4)) == None)
    assert(display.hexFromPixel(Vector2(20, 20)) == Some("(3,3)"))
    assert(display.hexFromPixel(Vector2(400, 24)) == None)
  }

  test("toPixel") {
    val grid = RectangularHexGrid.mutable(EvenHorizontalCoordSystem(), 4, 5, (c,r) => s"(${c},${r})")
    val display = HexGridDisplay(grid, 5)
    assert(display.toPixel(Coord.zero) == Vector2.zero)
    assert(display.toPixel(Coord(1, 2)) == Vector2(root3*5, 15))
  }
}