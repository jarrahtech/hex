package com.jarrahtechnology.hex

// if the direction option is None then standstill
final case class DirectionPath(val directions: Seq[Option[Direction]]) extends Iterable[Option[Direction]] {
  def length = directions.length
  require(length>0, "length>0")

  def iterator: Iterator[Option[Direction]] = directions.iterator

  /**
    * @param coords the coordinate system to use
    * @param start the coordinate from where the path should be followed
    * @return None if the path is not possible to follow in this coordinate system, otherwise the sequence of coordinates visited by following the this path
    */
  def toCoords(coords: CoordSystem)(start: Coord): Option[Seq[Coord]] = 
    directions.foldLeft((start, Option(List.empty[Coord])))((sofar, dir) => (sofar._1, sofar._2, dir) match {
        case (last, None, _) => sofar // no valid path previously
        case (last, Some(path), None) => (last, Some(path :+ last)) // standstill
        case (last, Some(path), Some(d)) => coords.neighbor(last)(d) match { // move one step
          case None => (last, None) // no valid path here
          case Some(next) => (next, Some(path :+ next)) // next position
        }        
      })._2.map(_.toSeq)

  /**
    * @param grid
    * @param start the coordinate from where the path should be followed
    * @return None if the path is not possible to follow on this grid's coordinate system or if any visited coordinate has no value, otherwise the sequence of coordinates and hexes visited by following this path
    */
  def toPath[H, C <: CoordSystem](grid: HexGrid[H, C])(start: Coord): Option[Seq[(Coord, H)]] = 
    toCoords(grid.coords)(start).flatMap(_.foldLeft(Option(List[(Coord, H)]()))((p, c) => (p, c) match {
      case (None, _) => None
      case (Some(path), coord) => grid.hexAt(coord) match {
        case None => None
        case Some(hex) => Some(path :+ (coord, hex))
      }
    }))

  /**
    * If don't care if the intermediate hexes on the path have values or not then use skipToPathEnd 
    * 
    * @param grid
    * @param start the coordinate from where the path should be followed
    * @return None if the path is not possible to follow on this grid's coordinate system or if any visited coordinate has no value (including the end), otherwise the coordinates and value of the destination of this path
    */
  def toPathEnd[H, C <: CoordSystem](grid: HexGrid[H, C])(start: Coord): Option[(Coord, H)] = toPath(grid)(start).flatMap(_.lastOption)

  /**
    * If need the intermediate hexes on the path to have values then use toPathEnd 
    *  
    * @param grid
    * @param start the coordinate from where the path should be followed
    * @return None if the path is not possible to follow on this grid's coordinate system or if the value of the destination has no value, otherwise the coordinate and value of the final destination visited by following the path (ignoring any intermediate hexes along the way)
    */
  def skipToPathEnd[H, C <: CoordSystem](grid: HexGrid[H, C])(start: Coord): Option[(Coord, H)] = 
    toCoords(grid.coords)(start).flatMap(_.lastOption.flatMap(c => grid.hexAt(c).map((c, _))))

  /**
    * If the directions are valid on this grid's coordinate system then apply the given function to the hexes visited on this path
    * 
    * @param grid
    * @param start the coordinate from where the directions should be followed
    * @param fn the function to apply to each visited hex, taking in (the sequential number of the step, the coordinate, and the value of the hex in the grid)
    */  
  def applyTo[H, C <: CoordSystem](grid: HexGrid[H, C])(start: Coord, fn: (Int, Coord, Option[H]) => Unit): Unit = 
    toCoords(grid.coords)(start).map(_.zipWithIndex).foreach(_.foreach(c => fn(c._2, c._1, grid.hexAt(c._1))))
}

object DirectionPath {
  val none = DirectionPath(List(None))
}
