package com.jarrahtechnology

import scala.scalajs.js.annotation._

package object hex {

  val root3 = math.sqrt(3)
  val root3div3 = root3/3f
  val twoThirds = 2/3f
  val hexSides = 6
  val sideAngle = 60
  val circleAngle = hexSides*sideAngle

  @JSExportAll
  enum Direction {
    case North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest

    private lazy val size = Direction.values.size
    def reverse = Direction.fromOrdinal((this.ordinal + size / 2) % size)
  }
}
