package com.jarrahtechnology.hex

import scala.collection.Seq as CommonSeq
import scala.collection.mutable.ArraySeq

/**
  * 
  *
  * @param coords
  * @param colRange inclusive from low -> high
  * @param rowRange inclusive from low -> high
  * @param hexes
  */
class RectangularHexGrid[+H, C <: CoordSystem](val coords: C, val colRange: (Int, Int), val rowRange: (Int, Int), val hexes: CommonSeq[CommonSeq[H]]) extends HexGrid[H, C] with Iterable[(Coord, H)] {
  assert(colRange._1 <= colRange._2 && rowRange._1 <= rowRange._2)
  assert(hexes.length == width)
  assert(!hexes.exists(_.length != height)) // force grid to be rectangular

  val width: Int = colRange._2 - colRange._1 + 1
  val height: Int = rowRange._2 - rowRange._1 + 1
  override val size: Int = width * height
  
  
  protected def get(pos: Coord): H = hexes(pos.column-colRange._1)(pos.row-rowRange._1)
  def valid(pos: Coord): Boolean = pos.column >= colRange._1 && pos.column <= colRange._2 && pos.row >= rowRange._1 && pos.row <= rowRange._2
  def hexAt(pos: Coord): Option[H] = if (valid(pos)) Some(get(pos)) else None

  private class LinearIterator(traverse: Int => (Int, Int)) extends Iterator[(Coord, H)] {
    @SuppressWarnings(Array("org.wartremover.warts.Var")) private var pos = -1;
    override def hasNext: Boolean = pos < RectangularHexGrid.this.size
    override def next(): (Coord, H) = {
      pos = pos + 1;
      val (col, row) = traverse(pos)
      (Coord(col+colRange._1, row+rowRange._1), hexes(col)(row))
    }
  }
  def colByRowIterator(): Iterator[(Coord, H)] = new LinearIterator(pos => (pos / width, pos % width))
  def rowByColIterator(): Iterator[(Coord, H)] = new LinearIterator(pos => (pos % height, pos / height))
  def iterator: Iterator[(Coord, H)] = colByRowIterator()
}

object RectangularHexGrid {
  import scala.reflect.ClassTag
  def immutable[H, C <: CoordSystem](coords: C, colRange: (Int, Int), rowRange: (Int, Int), generator: (Int, Int) => H) =
    ImmutableRectangularHexGrid(coords, colRange, rowRange, (colRange._1 to colRange._2).toList.map(c => (rowRange._1 to rowRange._2).toList.map(generator(c, _))))
  def mutable[H: ClassTag, C <: CoordSystem](coords: C, colRange: (Int, Int), rowRange: (Int, Int), generator: (Int, Int) => H) =
    MutableRectangularHexGrid[H, C](coords, colRange, rowRange, ArraySeq.tabulate(colRange._2 - colRange._1 + 1, rowRange._2 - rowRange._1 + 1)((c, r) => generator(c+colRange._1, r+rowRange._1)))
}

class ImmutableRectangularHexGrid[+H, C <: CoordSystem](coords: C, colRange: (Int, Int), rowRange: (Int, Int), hexes: List[List[H]])
    extends RectangularHexGrid[H, C](coords, colRange, rowRange, hexes) with ImmutableCoordinatedHexes[H, C, ImmutableRectangularHexGrid] {

  def set[T >: H](pos: Coord, h: T): ImmutableRectangularHexGrid[T, C] = {
    if (valid(pos)) new ImmutableRectangularHexGrid(coords, colRange, rowRange, hexes.updated(pos.column, hexes(pos.column-colRange._1).updated(pos.row-rowRange._1, h)))  
    else this
  }
  def set[T >: H](hs: Map[Coord, T]): ImmutableRectangularHexGrid[T, C] = {
    def valueAt(col: Int, row: Int) = hs.getOrElse(Coord(col, row), hexes(col)(row))
    new ImmutableRectangularHexGrid(coords, colRange, rowRange, (colRange._1 to colRange._2).toList.map(c => (rowRange._1 to rowRange._2).toList.map(r => valueAt(c-colRange._1, r-rowRange._1))))
  } 
}

class MutableRectangularHexGrid[+H, C <: CoordSystem](coords: C, colRange: (Int, Int), rowRange: (Int, Int), hexes: ArraySeq[ArraySeq[H]])
    extends RectangularHexGrid[H, C](coords, colRange, rowRange, hexes) with MutableCoordinatedHexes[H, C] {

  def set(pos: Coord, h: T): Unit = if (valid(pos)) hexes(pos.column).update(pos.row, h)
}
