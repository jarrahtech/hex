package com.jarrahtechnology.hex

trait HexGrid[+H, C <: CoordSystem] extends CoordinatedHexes[H, C] {
  def distance = coords.distance

  def neighbors(pos: Coord): List[H] = neighbors(pos, hexAt)
  def neighborsWithCoord(pos: Coord): List[(Coord, H)] = neighbors(pos, hexAtWithCoord)
  def neighbors[T](pos: Coord, f: (Coord) => Option[T]): List[T] = coords.neighbors(pos).map(f).flatten

  def range(center: Coord, distance: Int): IndexedSeq[H] = range(coords.toCube(center), distance, hexAt)
  def rangeWithCoord(center: Coord, distance: Int): IndexedSeq[(Coord, H)] = range(coords.toCube(center), distance, hexAtWithCoord)
  def range[T](center: CubeCoord, distance: Int, f: (Coord) => Option[T]): IndexedSeq[T] = for {
    q <- -distance to distance
    r <- math.max(-distance, -q - distance) to math.min(distance, -q + distance)
    h <- f(coords.toCoord(center + CubeCoord(q, r, -q - r)))
  } yield h

  def closest(from: Coord)(inZone: List[Coord]): Option[H] = findInZone(from, inZone, coords.closest, hexAt)
  def closestWithCoord(from: Coord)(inZone: List[Coord]): Option[(Coord, H)] = findInZone(from, inZone, coords.closest, hexAtWithCoord)
  def furthest(from: Coord)(inZone: List[Coord]): Option[H] = findInZone(from, inZone, coords.furthest, hexAt)
  def furthestWithCoord(from: Coord)(inZone: List[Coord]): Option[(Coord, H)] = findInZone(from, inZone, coords.furthest, hexAtWithCoord)
  def findInZone[T](from: Coord, inZone: List[Coord], find: Coord => List[Coord] => Option[Coord], trans: Coord => Option[T]): Option[T] = find(from)(inZone).flatMap(trans)
}
