package com.jarrahtechnology.hex

import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert.*

import scala.language.implicitConversions
import scala.language.postfixOps
import Direction.*

class DirectionTest extends TestCase {

  @Test def testReverse: Unit = {
    assertEquals(South, North.reverse)
    assertEquals(North, South.reverse)
    assertEquals(SouthEast, NorthWest.reverse)
    assertEquals(NorthWest, SouthEast.reverse)
  }

}
