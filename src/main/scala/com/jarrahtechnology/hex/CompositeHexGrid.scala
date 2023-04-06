package com.jarrahtechnology.hex

import scala.collection.mutable.Map
import scala.util.control.NonLocalReturns.*

/**
  * The C & C2 types are to force the grids to all have the same CoordSystem
  *
  * @param grids ordered
  */
trait CompositeHexGrid[+H, C <: CoordSystem, C2 >: C](grids: Seq[HexGrid[H, C & C2]]) extends HexGrid[H, C] {
  require(grids.length>1, "grids.length>1")
  val coords = grids(0).coords
  //require(!grids.exists(_.coords!=coords), s"all grids need the same coordinate system: $coords") // should not be necessary

  @SuppressWarnings(Array("org.wartremover.warts.Return")) 
  protected def findHexUptoGrid(pos: Coord, gridIdx: Int): Option[H] = returning {
    for (i <- 0 until gridIdx) do {
      val h = grids(i).hexAt(pos) 
      if (h.isDefined) throwReturn(h)
    }
    None
  }

  def hexAt(pos: Coord): Option[H] = findHexUptoGrid(pos, grids.length)

  def allHexAt(pos: Coord): Seq[H] = grids.map(_.hexAt(pos)).flatten

  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.Null"))
  private class CompositeIterator() extends Iterator[(Coord, H)] {
    private var currGridIterator = grids(0).iterator
    private var currGridIdx = 0
    private var current: (Coord, H) = null

    // get the next value to return from a next(), if any
    private def getNext: Unit = {
      if (current==null && currGridIdx<grids.length) { // don't have current and not past end of iterator        
        if (!currGridIterator.hasNext) { // this grid is finished, move to the next
          currGridIdx = currGridIdx+1
          if (currGridIdx<grids.length) {
            currGridIterator = grids(currGridIdx).iterator
            getNext
          }
        } else {
          val possible = currGridIterator.next
          if (findHexUptoGrid(possible._1, currGridIdx).isDefined) { // already since this coordinate, move to next
            getNext
          } else {
            current = possible
          }
        }
      }
    }

    def hasNext: Boolean = {
      if (current==null) getNext
      current!=null
    }
    def next(): (Coord, H) = 
      if (hasNext) {
        val result = current
        current = null
        result
      } else Iterator.empty.next()
  }

  def iterator: Iterator[(Coord, H)] = new CompositeIterator()
}

object CompositeHexGrid {
  def overlay[H, C <: CoordSystem](underlying: Seq[HexGrid[H, C]]) = {
    require(underlying.length>0, "underlying.length>0")
    HexGridOverlay(MutableSparseHexGrid(underlying(0).coords, collection.mutable.Map.empty[Coord, H]), underlying)
  }
}

// Mutable
final case class HexGridOverlay[H, C <: CoordSystem, C2 >: C](overlay: MutableCoordinatedHexes[H, C] with HexGrid[H, C & C2], underlying: Seq[HexGrid[H, C]]) 
    extends CompositeHexGrid[H, C, C2](overlay +: underlying)
    with MutableCoordinatedHexes[H, C] {

  def set(pos: Coord, h: H): Unit = overlay.set(pos, h)
}

final case class CompositeHexGrids[+H, C <: CoordSystem, C2 >: C](grids: Seq[HexGrid[H, C & C2]]) extends CompositeHexGrid[H, C, C2](grids)
