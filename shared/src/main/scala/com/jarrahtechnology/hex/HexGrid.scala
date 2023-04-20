package com.jarrahtechnology.hex

import scala.scalajs.js.annotation._

trait HexGrid[+H, C <: CoordSystem] extends Iterable[(Coord, H)] {
  def coords: C
  def hexAt(pos: Coord): Option[H]
  def distance = coords.distance

  def neighbor(pos: Coord, dir: Direction): Option[H] = neighbor(pos, dir, hexAt)
  def neighbor[T](pos: Coord, dir: Direction, f: (Coord) => Option[T]): Option[T] = coords.neighbor(pos)(dir).flatMap(f)
  def neighbors(pos: Coord): List[H] = neighbors(pos, hexAt)
  def neighbors[T](pos: Coord, f: (Coord) => Option[T]): List[T] = coords.neighbors(pos).map(f).flatten

  def range(center: Coord, distance: Int): IndexedSeq[H] = range(coords.toCube(center), distance, hexAt)
  def range[T](center: CubeCoord, distance: Int, f: (Coord) => Option[T]): IndexedSeq[T] = for {
    q <- -distance to distance
    r <- math.max(-distance, -q - distance) to math.min(distance, -q + distance)
    h <- f(coords.toCoord(center + CubeCoord(q, r, -q - r)))
  } yield h

  def closest(from: Coord)(inZone: List[Coord]): Option[H] = findInZone(from, inZone, coords.closest, hexAt)
  def furthest(from: Coord)(inZone: List[Coord]): Option[H] = findInZone(from, inZone, coords.furthest, hexAt)
  def findInZone[T](from: Coord, inZone: List[Coord], find: Coord => List[Coord] => Option[Coord], trans: Coord => Option[T]): Option[T] = find(from)(inZone).flatMap(trans)
}

trait ImmutableHexGrid[+H, C <: CoordSystem, +CH[X, Y <: CoordSystem] <: HexGrid[X, Y]] {
  // do nothing if pos not in the grid
  def set[T >: H](pos: Coord, h: T): CH[T, C]
  def set[T >: H](hs: Map[Coord, T]): CH[T, C] 
}

trait MutableHexGrid[H, C <: CoordSystem] {
  // do nothing if pos not in the grid
  def set(pos: Coord, h: H): Unit
  def set(hs: Map[Coord, H]): Unit = hs.foreach(h => set(h._1, h._2))
}

@JSExportAll
object HexGrid {
  def union[H, C <: CoordSystem](grid1: HexGrid[H, C], grid2: HexGrid[H, C]) = CompositeHexGrids(List(grid1, grid2))
  def intersection[H, C <: CoordSystem, C2 >: C](grid1: HexGrid[H, C & C2], grid2: HexGrid[H, C & C2]) = 
    // take hexes from first grid in preference
    ImmutableSparseHexGrid(grid1.coords, grid1.iterator.foldLeft(Map[Coord, H]())((m, h) => if (grid2.hexAt(h._1).isDefined) m.updated(h._1, h._2) else m))
}