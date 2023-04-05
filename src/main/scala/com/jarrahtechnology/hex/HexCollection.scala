package com.jarrahtechnology.hex

// Immutable
final case class HexCollection[+H, C <: CoordSystem](val coords: C, val hexes: collection.immutable.Map[Coord, H]) 
    extends SparseHexes[H, C](coords, hexes)
    with ImmutableCoordinatedHexes[H, C, HexCollection] {

  def set[T >: H](pos: Coord, h: T): HexCollection[T, C] = HexCollection(coords, hexes.updated(pos, h))
  def set[T >: H](hs: Map[Coord, T]): HexCollection[T, C] = HexCollection(coords, hexes ++ hs)
}
