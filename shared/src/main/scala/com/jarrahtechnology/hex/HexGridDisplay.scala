package com.jarrahtechnology.hex

import com.jarrahtechnology.util.Vector2
import scala.scalajs.js.annotation._

@JSExportAll
final case class HexGridDisplay[H, C <: CoordSystem](val grid: HexGrid[H, C], val hexRadius: Double) {
  def fromPixel(pos: Vector2) = grid.coords.fromRadii(pos.divide(hexRadius))
  def hexFromPixel(pos: Vector2) = grid.hexAt(fromPixel(pos))
  def toPixel(coord: Coord) = grid.coords.toRadii(coord).multiply(hexRadius)
}

@JSExportAll
object HexGridDisplay {
  @JSExport("apply") def apply[H, C <: CoordSystem](grid: HexGrid[H, C]): HexGridDisplay[H, C] = HexGridDisplay(grid, 1d)
}