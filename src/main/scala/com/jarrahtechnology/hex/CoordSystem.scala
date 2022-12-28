package com.jarrahtechnology.hex

object CoordSystem {
  enum Direction {
    case North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest

    private val size =Direction.values.size
    def reverse =Direction.fromOrdinal((this.ordinal + size / 2) % size)
  }
}

trait CoordSystem {
  import CoordSystem.Direction

  def isHorizontal: Boolean
  def isEven: Boolean
  def validDirections: List[Direction] // in clockwise order
  def neighborShift(pos: Coord)(dir: Option[Direction]): Coord
  protected def neighborShifts: List[List[Coord]]
  def toCube(c: Coord): CubeCoord
  def toCoord(c: CubeCoord): Coord

  def rotateClockwise(dir: Option[Direction]) = rotate(1, dir)
  def rotateAnticlockwise(dir: Option[Direction]) = rotate(Hex.sides - 1, dir)
  private def rotate(shift: Int, dir: Option[Direction]) = dir.map(validDirections.indexOf(_)) match {
    case Some(d) if d > -1 => Some(validDirections((d + shift) % Hex.sides))
    case _                 => None
  }
  def neighbor(pos: Coord)(direction: Option[Direction]) = pos + neighborShift(pos)(direction)
  def distance(c1: Coord)(c2: Coord) = toCube(c1).distance(toCube(c2))
}

trait HorizontalCoordSystem extends CoordSystem {
  import CoordSystem.Direction
  import CoordSystem.Direction.*

  val isHorizontal = true
  val validDirections = List(NorthEast, East, SouthEast, SouthWest, West, NorthWest)
  def neighborShift(pos: Coord)(dir: Option[Direction]) = dir match {
    case Some(d) => neighborShifts(pos.y & 1)(d.ordinal);
    case None    => Coord.zero
  }
}

trait VerticalCoordSystem extends CoordSystem {
  import CoordSystem.Direction
  import CoordSystem.Direction._

  val isHorizontal = false
  val validDirections = List(North, NorthEast, SouthEast, South, SouthWest, NorthWest)
  def neighborShift(pos: Coord)(dir: Option[Direction]) = dir match {
    case Some(d) => neighborShifts(pos.x & 1)(d.ordinal);
    case None    => Coord.zero
  }
}

sealed case class EvenHorizontalCoordSystem() extends HorizontalCoordSystem {
  val isEven = true
  def neighborShifts = List(
    List(Coord.zero, Coord(1, 1), Coord(1, 0), Coord(1, -1), Coord.zero, Coord(0, -1), Coord(-1, 0), Coord(0, 1)),
    List(Coord.zero, Coord(0, 1), Coord(1, 0), Coord(0, -1), Coord.zero, Coord(-1, -1), Coord(-1, 0), Coord(-1, 1))
  )
  def toCube(c: Coord) = {
    val x = c.x - (c.y + (c.y & 1)) / 2
    val z = c.y
    CubeCoord(x, -x - z, z)
  }
  def toCoord(c: CubeCoord) = Coord(c.x + (c.z + (c.z & 1)) / 2, c.z)
}

sealed case class EvenVerticalCoordSystem() extends VerticalCoordSystem {
  val isEven = true
  def neighborShifts = List(
    List(Coord(0, 1), Coord(1, 1), Coord.zero, Coord(1, 0), Coord(0, -1), Coord.zero, Coord(-1, 0), Coord(-1, 1)),
    List(Coord(0, 1), Coord(1, 0), Coord.zero, Coord(1, -1), Coord(0, -1), Coord.zero, Coord(-1, -1), Coord(-1, 0))
  )
  def toCube(c: Coord) = {
    val x = c.x
    val z = c.y - (c.x + (c.x & 1)) / 2
    CubeCoord(x, -x - z, z)
  }
  def toCoord(c: CubeCoord) = Coord(c.x, c.z + (c.x + (c.x & 1)) / 2)
}

sealed case class OddHorizontalCoordSystem() extends HorizontalCoordSystem {
  val isEven = false
  def neighborShifts = List(
    List(Coord.zero, Coord(0, 1), Coord(1, 0), Coord(0, -1), Coord.zero, Coord(-1, -1), Coord(-1, 0), Coord(-1, 1)),
    List(Coord.zero, Coord(1, 1), Coord(1, 0), Coord(1, -1), Coord.zero, Coord(0, -1), Coord(-1, 0), Coord(0, 1))
  )
  def toCube(c: Coord) = {
    val x = c.x - (c.y - (c.y & 1)) / 2
    val z = c.y
    CubeCoord(x, -x - z, z)
  }
  def toCoord(c: CubeCoord) = Coord(c.x + (c.z - (c.z & 1)) / 2, c.z)
}

sealed case class OddVerticalCoordSystem() extends VerticalCoordSystem {
  val isEven = false
  def neighborShifts = List(
    List(Coord(0, 1), Coord(1, 0), Coord.zero, Coord(1, -1), Coord(0, -1), Coord.zero, Coord(-1, -1), Coord(-1, 0)),
    List(Coord(0, 1), Coord(1, 1), Coord.zero, Coord(1, 0), Coord(0, -1), Coord.zero, Coord(-1, 0), Coord(-1, 1))
  )
  def toCube(c: Coord) = {
    val x = c.x
    val z = c.y - (c.x - (c.x & 1)) / 2
    CubeCoord(x, -x - z, z)
  }
  def toCoord(c: CubeCoord) = Coord(c.x, c.y + (c.x - (c.x & 1)) / 2)
}
