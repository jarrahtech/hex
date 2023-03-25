package com.jarrahtechnology.hex

// TODO: return cell too
trait CoordinatedHexes[+H, C <: CoordSystem] {
  type T <: H
  type Cell = (Coord, T)

  def coords: C
  def hexAt(pos: Coord): Option[H]
  def hexAt(col: Int, row: Int): Option[H]

  //def zip(pos: Coord): Option[Cell]
  //def zip(col: Int, row: Int): Option[Cell]
}

// TODO: use this
trait CoordinatedHexOps[+H, C <: CoordSystem, +CH[X, Y <: CoordSystem] <: CoordinatedHexes[X, Y], +Z <: CoordinatedHexes[H, C] with CoordinatedHexOps[H, C, CH, Z]] {
  def map[T](f: (H) => T): CH[T, C]
  def flatMap[T](f: (H) => IterableOnce[T]): CH[T, C]
  def foreach[U](f: (H) => U): Unit
  def filter(p: (H) => Boolean): Z
  def fold[H1 >: H](z: H1)(op: (H1, H1) => H1): H1
  def find(p: (H) => Boolean): Option[H]
  def exists(p: (H) => Boolean): Boolean = find(p).isDefined

  /* TODO:
  private class LinearIterator(traverse: Int => (Int, Int)) extends Iterator[Hex[H]] {
    @SuppressWarnings(Array("org.wartremover.warts.Var")) private var pos = -1;
    override def hasNext: Boolean = pos < HexGrid.this.size
    override def next(): Hex[H] = {
      pos = pos + 1;
      HexGrid.this.apply.tupled(traverse(pos))
    }
  }
  def colByRow(): Iterator[Hex[H]] = new LinearIterator(pos => (pos / width, pos % width))
  def rowByCol(): Iterator[Hex[H]] = new LinearIterator(pos => (pos % height, pos / height))
 */
}

trait ImmutableCoordinatedHexes[+H, C <: CoordSystem, +CH[X, Y <: CoordSystem] <: CoordinatedHexes[X, Y]] {
  // do nothing if pos not in the grid
  def set[T >: H](pos: Coord, h: T): CH[T, C]
  def set[T >: H](hs: Map[Coord, T]): CH[T, C] 
}

trait MutableCoordinatedHexes[+H, C <: CoordSystem] {
  type T <: H
  // do nothing if pos not in the grid
  def set(pos: Coord, h: T): Unit
  def set(hs: Map[Coord, T]): Unit = hs.foreach(h => set(h._1, h._2))
}