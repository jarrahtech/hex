package com.jarrahtechnology.hex

import scala.scalajs.js.annotation._

@JSExportAll
object Flood {
  def fill[H, C <: CoordSystem](grid: HexGrid[H, C])(origin: Coord, dist: Int): HexGrid[H, C] = fill(grid, grid.hexAt)(origin, dist)
  
  def fill[H, C <: CoordSystem, T](grid: HexGrid[H, C], valid: (Coord) => Option[T])(origin: Coord, dist: Int) = {
    require(dist>=0, "distance>=0")
    import collection.mutable.ArrayBuffer
    val fringes = ArrayBuffer(ArrayBuffer(origin))
    val visited = collection.mutable.Map(origin -> valid(origin).getOrElse(throw IllegalArgumentException("origin must be valid")))
    for {
      d <- 1 to dist
      _ = fringes.addOne(ArrayBuffer.empty)
      f <- fringes(d-1)
      c <- grid.coords.neighbors(f) 
      h <- valid(c) if !visited.contains(c)
    } do {
      visited.addOne(c -> h)
      fringes(d).addOne(c)
    }
    ImmutableSparseHexGrid(grid.coords, visited.toMap)
  }  
}
