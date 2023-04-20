package com.jarrahtechnology.hex

import scala.scalajs.js.annotation._
import scala.collection.Map as CommonMap

trait SparseHexGrid[+H, C <: CoordSystem](coords: C, hexes: CommonMap[Coord, H]) extends HexGrid[H, C] {
  override def size: Int = hexes.size
  def hexAt(pos: Coord): Option[H] = hexes.get(pos)
  def iterator: Iterator[(Coord, H)] = hexes.iterator
}

@JSExportAll
final case class ImmutableSparseHexGrid[+H, C <: CoordSystem](val coords: C, val hexes: collection.immutable.Map[Coord, H])
    extends SparseHexGrid[H, C](coords, hexes) 
    with HexGrid[H, C] 
    with ImmutableHexGrid[H, C, ImmutableSparseHexGrid] {

  def set[T >: H](pos: Coord, h: T): ImmutableSparseHexGrid[T, C] = ImmutableSparseHexGrid(coords, hexes.updated(pos, h))
  def set[T >: H](hs: Map[Coord, T]): ImmutableSparseHexGrid[T, C] = ImmutableSparseHexGrid(coords, hexes ++ hs)
}

@JSExportAll
final case class MutableSparseHexGrid[H, C <: CoordSystem](val coords: C, val hexes: collection.mutable.Map[Coord, H])
    extends SparseHexGrid[H, C](coords, hexes) 
    with HexGrid[H, C] 
    with MutableHexGrid[H, C] {
  
  def set(pos: Coord, h: H): Unit = hexes.update(pos, h)
}