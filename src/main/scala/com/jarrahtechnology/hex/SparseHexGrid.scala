package com.jarrahtechnology.hex

final case class ImmutableSparseHexGrid[+H, C <: CoordSystem](val coords: C, val hexes: collection.immutable.Map[Coord, H])
    extends SparseHexes[H, C](coords, hexes) 
    with HexGrid[H, C] 
    with ImmutableCoordinatedHexes[H, C, ImmutableSparseHexGrid] {

  def set[T >: H](pos: Coord, h: T): ImmutableSparseHexGrid[T, C] = ImmutableSparseHexGrid(coords, hexes.updated(pos, h))
  def set[T >: H](hs: Map[Coord, T]): ImmutableSparseHexGrid[T, C] = ImmutableSparseHexGrid(coords, hexes ++ hs)
}

final case class MutableSparseHexGrid[H, C <: CoordSystem](val coords: C, val hexes: collection.mutable.Map[Coord, H])
    extends SparseHexes[H, C](coords, hexes) 
    with HexGrid[H, C] 
    with MutableCoordinatedHexes[H, C] {
  
  def set(pos: Coord, h: H): Unit = hexes.update(pos, h)
}