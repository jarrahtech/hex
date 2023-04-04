package com.jarrahtechnology.hex

// Immutable
case class HexCollection[+H, C <: CoordSystem](val coords: C, val hexes: Map[Coord, H]) extends CoordinatedHexes[H, C]  with ImmutableCoordinatedHexes[H, C, HexCollection] {
  def hexAt(pos: Coord): Option[H] = hexAtWithCoord(pos).map(_._2)
  override def hexAtWithCoord(pos: Coord): Option[(Coord, H)] = hexes.find(_._1==pos)
  def iterator: Iterator[(Coord, H)] = hexes.iterator
  def set[T >: H](pos: Coord, h: T): HexCollection[T, C] = HexCollection(coords, hexes.updated(pos, h))
  def set[T >: H](hs: Map[Coord, T]): HexCollection[T, C] = HexCollection(coords, hexes ++ hs)
}
