package com.jarrahtechnology.hex

trait CoordinatedHexes[+H, C <: CoordSystem] extends Iterable[(Coord, H)] {
  def coords: C
  def hexAt(pos: Coord): Option[H]
  def hexAtWithCoord(pos: Coord): Option[(Coord, H)] = hexAt(pos).map((pos, _))
}

trait ImmutableCoordinatedHexes[+H, C <: CoordSystem, +CH[X, Y <: CoordSystem] <: CoordinatedHexes[X, Y]] {
  // do nothing if pos not in the grid
  def set[T >: H](pos: Coord, h: T): CH[T, C]
  def set[T >: H](hs: Map[Coord, T]): CH[T, C] 
}

trait MutableCoordinatedHexes[H, C <: CoordSystem] {
  // do nothing if pos not in the grid
  def set(pos: Coord, h: H): Unit
  def set(hs: Map[Coord, H]): Unit = hs.foreach(h => set(h._1, h._2))
}