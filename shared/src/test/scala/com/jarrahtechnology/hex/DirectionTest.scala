package com.jarrahtechnology.hex

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions
import scala.language.postfixOps
import Direction.*

class DirectionTest extends AnyFunSuite {

  test("Reverse") {
    assert(South==North.reverse)
    assert(North==South.reverse)
    assert(SouthEast==NorthWest.reverse)
    assert(NorthWest==SouthEast.reverse)
  }

}
