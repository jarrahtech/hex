package com.jarrahtechnology

import scala.scalajs.js.annotation._
import com.jarrahtechnology.util.Vector2

package object hex {

  val root3 = math.sqrt(3)
  val root3div3 = root3/3f
  val twoThirds = 2/3f
  val hexSides = 6
  val sideAngle = 60
  val circleAngle = hexSides*sideAngle

  val flatTopHexPixelPoints = List(Vector2(0, root3/4d), Vector2(0.25, root3/2d), Vector2(0.75, root3/2d), Vector2(1, root3/4d), Vector2(0.75, 0), Vector2(0.25, 0))
  val pointyTopHexPixelPoints = List(Vector2(0, 0.25), Vector2(0, 0.75), Vector2(root3/4d, 1), Vector2(root3/2d, 0.75), Vector2(root3/2d, 0.25), Vector2(root3/4d, 0))

  @JSExportAll
  enum Direction {
    case North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest

    private lazy val size = Direction.values.size
    def reverse = Direction.fromOrdinal((this.ordinal + size / 2) % size)
  }
}
