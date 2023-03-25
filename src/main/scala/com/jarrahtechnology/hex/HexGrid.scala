package com.jarrahtechnology.hex

trait HexGrid[+H, C <: CoordSystem] extends CoordinatedHexes[H, C] {
  def distance = coords.distance
  def neighbors(pos: Coord): List[Option[H]] = coords.neighbors(pos).map(hexAt)

  def range(center: Coord, distance: Int): IndexedSeq[H] = range(coords.toCube(center), distance)
  def range(center: CubeCoord, distance: Int) = for {
    x <- -distance to distance
    y <- math.max(-distance, -x - distance) to math.max(distance, -x + distance)
    h <- hexAt(coords.toCoord(center + CubeCoord(x, y, -x - y)))
  } yield h

  def closest(from: Coord)(zone: List[Coord]): Option[H] = coords.closest(from)(zone).flatMap(hexAt)
  def furthest(from: Coord)(zone: List[Coord]): Option[H] = coords.furthest(from)(zone).flatMap(hexAt)
}
