package com.jarrahtechnology.hex

import scala.scalajs.js.annotation._
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
trait RectangularHexGrid[+H, C <: CoordSystem](coords: C, colRange: (Int, Int), rowRange: (Int, Int), hexes: CommonSeq[CommonSeq[H]]) 
    extends HexGrid[H, C] with RectangularGrid {
  
  override val size: Int = capacity

  require(hexes.length == numColumns, s"provided hexes wrong size: length=${hexes.length}, width=${numColumns}")
  require(!hexes.exists(_.length != numRows), s"grid not rectangular: height=${numRows}")
  
  protected def get(pos: Coord): H = hexes(pos.column-colRange._1)(pos.row-rowRange._1)
  def hexAt(pos: Coord): Option[H] = if (inBounds(pos)) Some(get(pos)) else None

  private class LinearIterator(traverse: Int => (Int, Int)) extends Iterator[(Coord, H)] {
    @SuppressWarnings(Array("org.wartremover.warts.Var")) private var pos = 0;
    def hasNext: Boolean = pos < RectangularHexGrid.this.size
    def next(): (Coord, H) = {
      val (col, row) = traverse(pos)
      pos = pos + 1;
      (Coord(col+colRange._1, row+rowRange._1), hexes(col)(row))
    }
  }
  def colByRowIterator(): Iterator[(Coord, H)] = new LinearIterator(pos => (pos / numRows, pos % numRows))
  def rowByColIterator(): Iterator[(Coord, H)] = new LinearIterator(pos => (pos % numColumns, pos / numColumns))
  def iterator: Iterator[(Coord, H)] = colByRowIterator()
}

@JSExportAll
object RectangularHexGrid {
  import scala.reflect.ClassTag
  
  def immutable[H, C <: CoordSystem](coords: C, maxCol: Int, maxRow: Int, generator: (Int, Int) => H): ImmutableRectangularHexGrid[H, C] = 
    immutable(coords, (0, maxCol), (0, maxRow), generator)
  def immutable[H, C <: CoordSystem](coords: C, colRange: (Int, Int), rowRange: (Int, Int), generator: (Int, Int) => H) =
    ImmutableRectangularHexGrid(coords, colRange, rowRange, (colRange._1 to colRange._2).toList.map(c => (rowRange._1 to rowRange._2).toList.map(generator(c, _))))
  
  def mutable[H: ClassTag, C <: CoordSystem](coords: C, maxCol: Int, maxRow: Int, generator: (Int, Int) => H): MutableRectangularHexGrid[H, C] = 
    mutable(coords, (0, maxCol), (0, maxRow), generator)
  def mutable[H: ClassTag, C <: CoordSystem](coords: C, colRange: (Int, Int), rowRange: (Int, Int), generator: (Int, Int) => H) =
    MutableRectangularHexGrid[H, C](coords, colRange, rowRange, ArraySeq.tabulate(colRange._2 - colRange._1 + 1, rowRange._2 - rowRange._1 + 1)((c, r) => generator(c+colRange._1, r+rowRange._1)))
}

@JSExportAll
final case class ImmutableRectangularHexGrid[+H, C <: CoordSystem](val coords: C, val colRange: (Int, Int), val rowRange: (Int, Int), val hexes: List[List[H]])
    extends RectangularHexGrid[H, C](coords, colRange, rowRange, hexes) 
    with ImmutableHexGrid[H, C, ImmutableRectangularHexGrid] 
    with RectangularGrid(colRange, rowRange) {

  def set[T >: H](pos: Coord, h: T): ImmutableRectangularHexGrid[T, C] = {
    if (inBounds(pos)) ImmutableRectangularHexGrid(coords, colRange, rowRange, hexes.updated(pos.column-colRange._1, hexes(pos.column-colRange._1).updated(pos.row-rowRange._1, h)))  
    else this
  }
  def set[T >: H](hs: Map[Coord, T]): ImmutableRectangularHexGrid[T, C] = {
    def valueAt(pos: Coord) = hs.getOrElse(pos, get(pos))
    ImmutableRectangularHexGrid(coords, colRange, rowRange, (colRange._1 to colRange._2).toList.map(c => (rowRange._1 to rowRange._2).toList.map(r => valueAt(Coord(c, r)))))
  } 
}

@JSExportAll
final case class MutableRectangularHexGrid[H, C <: CoordSystem](val coords: C, val colRange: (Int, Int), val rowRange: (Int, Int), val hexes: ArraySeq[ArraySeq[H]])
    extends RectangularHexGrid[H, C](coords, colRange, rowRange, hexes) 
    with MutableHexGrid[H, C]
    with RectangularGrid(colRange, rowRange) {
  
  def set(pos: Coord, h: H): Unit = if (inBounds(pos)) hexes(pos.column-colRange._1).update(pos.row-rowRange._1, h)
}
