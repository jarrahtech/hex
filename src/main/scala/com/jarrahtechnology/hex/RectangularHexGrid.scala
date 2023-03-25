package com.jarrahtechnology.hex

import scala.collection.Seq as CommonSeq
import scala.collection.mutable.ArraySeq

/**
  * 
  *
  * @param coords
  * @param colRange inclusive
  * @param rowRange inclusive
  * @param hexes
  */
class RectangularHexGrid[+H, C <: CoordSystem](val coords: C, val colRange: (Int, Int), val rowRange: (Int, Int), val hexes: CommonSeq[CommonSeq[H]]) extends HexGrid[H, C] {
  assert(hexes.length == width)
  assert(!hexes.exists(_.length != height)) // force grid to be rectangular

  val width: Int = colRange._2 - colRange._1 + 1
  val height: Int = rowRange._2 - rowRange._1 + 1
  val size: Int = width * height
  def valid(col: Int, row: Int): Boolean = col >= colRange._1 && col <= colRange._2 && row >= rowRange._1 && row <= rowRange._2
  
  def hexAt(pos: Coord): Option[H] = hexAt(pos.column, pos.row)
  def hexAt(col: Int, row: Int): Option[H] = if (valid(col, row)) Some(hexes(col)(row)) else None
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
    if (valid(pos.column, pos.row)) new ImmutableRectangularHexGrid(coords, colRange, rowRange, hexes.updated(pos.column, hexes(pos.column).updated(pos.row, h)))  
    else this
   }
  def set[T >: H](hs: Map[Coord, T]): ImmutableRectangularHexGrid[T, C] = {
    def valueAt(col: Int, row: Int) = hs.getOrElse(Coord(col, row), hexes(col)(row))
    new ImmutableRectangularHexGrid(coords, colRange, rowRange, (colRange._1 to colRange._2).toList.map(c => (rowRange._1 to rowRange._2).toList.map(valueAt(c, _))))
  } 
}

class MutableRectangularHexGrid[+H, C <: CoordSystem](coords: C, colRange: (Int, Int), rowRange: (Int, Int), hexes: ArraySeq[ArraySeq[H]])
   extends RectangularHexGrid[H, C](coords, colRange, rowRange, hexes) with MutableCoordinatedHexes[H, C] {

  def set(pos: Coord, h: T): Unit = if (valid(pos.column, pos.row)) hexes(pos.column).update(pos.row, h)
}
