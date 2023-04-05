package com.jarrahtechnology.hex

import scala.collection.Map as CommonMap

trait SparseHexes[+H, C <: CoordSystem](coords: C, hexes: CommonMap[Coord, H]) extends CoordinatedHexes[H, C] {
  override def size: Int = hexes.size
  def hexAt(pos: Coord): Option[H] = hexes.get(pos)
  override def hexAtWithCoord(pos: Coord): Option[(Coord, H)] = hexes.find(_._1==pos)
  def iterator: Iterator[(Coord, H)] = hexes.iterator
}
