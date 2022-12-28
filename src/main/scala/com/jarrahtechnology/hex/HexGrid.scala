package com.jarrahtechnology.hex

sealed case class Hex[H](position: Coord, value: Option[H]) {}

object Hex {
  val sides = 6;
}

/* https://www.redblobgames.com/grids/hexagons/
space save, time save impl
overlay
tests
rings
spirals
chain
flood
a*
fov
 */
trait HexGrid[H] {

  def widthRange: (Int, Int) // (min, max)
  def heightRange: (Int, Int) // (min, max)
  def width: Int = widthRange._2 - widthRange._1 + 1
  def height: Int = heightRange._2 - heightRange._1 + 1
  def size: Int = width * height
  def valid(pos: Coord): Boolean = pos.x >= widthRange._1 && pos.x <= widthRange._2 && pos.y >= heightRange._1 && pos.y <= heightRange._2

  def coord: CoordSystem

  def map[T](m: Hex[H] => T): HexGrid[T]

  @throws[IndexOutOfBoundsException]
  def apply(pos: Coord): Hex[H] = apply(pos.x, pos.y)
  @throws[IndexOutOfBoundsException]
  def apply(x: Int, y: Int): Hex[H]
  def lift(pos: Coord): Option[Hex[H]] = if (valid(pos)) Some(apply(pos)) else None

  def distance(from: Coord, to: Coord): Int = coord.distance(from)(to)

  def getNeighbors(pos: Coord): List[Hex[H]] = getNeighbors(pos, coord.neighborShift(pos))
  private def getNeighbors(pos: Coord, neighborFn: Option[CoordSystem.Direction] => Coord) = for {
    d <- coord.validDirections
    h <- lift(pos + neighborFn(Some(d)))
  } yield h

  private class LinearIterator(traverse: Int => (Int, Int)) extends Iterator[Hex[H]] {
    private var pos = -1;
    override def hasNext: Boolean = pos < HexGrid.this.size
    override def next(): Hex[H] = {
      pos = pos + 1;
      HexGrid.this.apply.tupled(traverse(pos))
    }
  }
  def colByRow(): Iterator[Hex[H]] = new LinearIterator(pos => (pos / width, pos % width))
  def rowByCol(): Iterator[Hex[H]] = new LinearIterator(pos => (pos % height, pos / height))

  def range(center: Coord, distance: Int): IndexedSeq[Hex[H]] = range(coord.toCube(center), distance)
  private def range(center: CubeCoord, distance: Int) = for {
    x <- -distance to distance
    y <- math.max(-distance, -x - distance) to math.max(distance, -x + distance)
    h <- lift(coord.toCoord(center + CubeCoord(x, y, -x - y)))
  } yield h

  def closest(from: Hex[H], zone: List[Hex[H]]): Option[Hex[H]] =
    zone.foldRight((None: Option[Hex[H]], Int.MaxValue, Double.MaxValue))((h, c) => {
        val dist = distance(from.position, h.position)
        val lineDist = math.pow(h.position.x - from.position.x, 2) + math.pow(h.position.y - from.position.y, 2);
        if (dist < c._2 || (dist == c._2 && lineDist < c._3)) {
          (Some(h), dist, lineDist)
        } else {
          c
        }
      })
      ._1
}
