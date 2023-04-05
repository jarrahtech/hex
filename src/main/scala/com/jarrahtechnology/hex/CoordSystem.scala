package com.jarrahtechnology.hex

trait CoordSystem {
  def isHorizontal: Boolean
  def isEven: Boolean
  def validDirections: List[Direction] // in clockwise order 
  protected def neighborShifts: List[List[Option[Coord]]]
  def toCube(c: Coord): CubeCoord
  def toCoord(c: CubeCoord): Coord
  protected def neighborLine(pos: Coord): Int

  def rotateClockwise(dir: Direction): Option[Direction] = rotate(dir)(1)
  def rotateAnticlockwise(dir: Direction): Option[Direction] = rotate(dir)(hexSides - 1)
  def rotate(dir: Direction)(shift: Int): Option[Direction] = validDirections.indexOf(dir) match {
    case d if d >= 0 => Some(validDirections((d + shift) % hexSides)) // assumes validDirections in clockwise order
    case _           => None
  }
  
  def neighborShift(pos: Coord)(dir: Direction): Option[Coord] = neighborShifts(neighborLine(pos))(dir.ordinal)
  def neighbor(pos: Coord)(direction: Direction): Option[Coord] = neighborShift(pos)(direction).map(_ + pos)
  def neighbors(pos: Coord): List[Coord] = neighborShifts(neighborLine(pos)).flatten.map(_ + pos)

  def distance(c1: Coord)(c2: Coord) = toCube(c1).distance(toCube(c2))
  def closest(from: Coord)(zone: List[Coord]): Option[Coord] = zone.minByOption(distance(from)(_))
  def furthest(from: Coord)(zone: List[Coord]): Option[Coord] = zone.maxByOption(distance(from)(_))
}

private sealed trait HorizontalCoordSystem extends CoordSystem { // pointy tops
  import Direction.*
  def isHorizontal = true
  def validDirections = List(NorthEast, East, SouthEast, SouthWest, West, NorthWest)
  protected def neighborLine(pos: Coord) = pos.y & 1
}

private sealed trait VerticalCoordSystem extends CoordSystem { // flat tops
  import Direction.*
  def isHorizontal = false
  def validDirections = List(North, NorthEast, SouthEast, South, SouthWest, NorthWest)
  protected def neighborLine(pos: Coord) = pos.x & 1
}

final case class EvenHorizontalCoordSystem() extends HorizontalCoordSystem {
  import Coord.*
  val isEven = true
  protected def neighborShifts = List(
    List(None, Option(northEast), Option(east), Option(southEast), None, Option(south), Option(west), Option(north)),
    List(None, Option(north), Option(east), Option(south), None, Option(southWest), Option(west), Option(northWest))
  )
  def toCube(c: Coord) = CubeCoord(c.x - (c.y + (c.y & 1)) / 2, c.y)
  def toCoord(c: CubeCoord) = Coord(c.q + (c.r + (c.r & 1)) / 2, c.r)
}

final case class EvenVerticalCoordSystem() extends VerticalCoordSystem {
  import Coord.*
  val isEven = true
  protected def neighborShifts = List(
    List(Option(north), Option(northEast), None, Option(east), Option(south), Option(west), None, Option(northWest)),
    List(Option(north), Option(east), None, Option(southEast), Option(south), Option(southWest), None, Option(west))
  )
  def toCube(c: Coord) = CubeCoord(c.x, c.y - (c.x + (c.x & 1)) / 2)
  def toCoord(c: CubeCoord) = Coord(c.q, c.r + (c.q + (c.q & 1)) / 2)
}

final case class OddHorizontalCoordSystem() extends HorizontalCoordSystem {
  import Coord.*
  val isEven = false
  protected def neighborShifts = List(
    List(None, Option(north), Option(east), Option(south), None, Option(southWest), Option(west), Option(northWest)),
    List(None, Option(northEast), Option(east), Option(southEast), None, Option(south), Option(west), Option(north))
  )
  def toCube(c: Coord) = CubeCoord(c.x - (c.y - (c.y & 1)) / 2, c.y)
  def toCoord(c: CubeCoord) = Coord(c.q + (c.r - (c.r & 1)) / 2, c.r)
}

final case class OddVerticalCoordSystem() extends VerticalCoordSystem {
  import Coord.*
  val isEven = false
  protected def neighborShifts = List(
    List(Option(north), Option(east), None, Option(southEast), Option(south), Option(southWest), None, Option(west)),
    List(Option(north), Option(northEast), None, Option(east), Option(south), Option(west), None, Option(northWest))
  )
  def toCube(c: Coord) = CubeCoord(c.x, c.y - (c.x - (c.x & 1)) / 2)
  def toCoord(c: CubeCoord) = Coord(c.q, c.r + (c.q - (c.q & 1)) / 2)
}

object CoordSystem {
  val evenVertical = EvenVerticalCoordSystem()
  val evenHorizontal = EvenHorizontalCoordSystem()
  val oddVertical = OddVerticalCoordSystem()
  val oddHorizontal = OddHorizontalCoordSystem()
}