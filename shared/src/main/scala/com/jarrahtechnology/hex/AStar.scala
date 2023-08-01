package com.jarrahtechnology.hex

private type CoordCost = (Int, Coord)

final case class AStarMap(origin: Coord, paths: Map[Coord, Coord]) {
  def hasPathTo(destination: Coord) = paths.contains(destination)
  def pathTo(destination: Coord): Seq[Coord] = {
    def getPath(c: Coord): Seq[Coord] = paths.get(c) match {
      case None => List.empty
      case Some(value) => getPath(value) :+ value
    }
    if (hasPathTo(destination)) getPath(destination) :+ destination else List.empty
  }
}

object AStar {
  private val frontierOrdering = Ordering.by[CoordCost, Int](-_._1)

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def neighborsCost1(grid: HexGrid[?, ?])(cc: CoordCost) = grid.neighbors(cc._2, c => grid.hexAt(c).map(? => (cc._1+1, c))) // take neighbors if exist and all moves cost 1
  
  def sparseNeighborsCost1(grid: HexGrid[?, ?])(cc: CoordCost) = grid.neighbors(cc._2, Some(cc._1+1, _)) // take neighbors even if don't exist and all moves cost 1
  def untilAllChecked(paths: collection.Map[Coord, Coord]) = true
  def untilReach(dest: Coord)(paths: collection.Map[Coord, Coord]) = !paths.contains(dest)

  def buildFrom(grid: HexGrid[?, ?], origin: Coord): AStarMap = buildFrom(origin, neighborsCost1(grid), untilAllChecked)

  def buildFrom(origin: Coord, neighbors: CoordCost => Seq[CoordCost], continue: collection.Map[Coord, Coord] => Boolean): AStarMap = {
    val costSoFar = collection.mutable.HashMap(origin -> 0)
    val paths = collection.mutable.HashMap.empty[Coord, Coord]
    val frontier = collection.mutable.PriorityQueue((0, origin))(frontierOrdering) // lowest priority first // TODO: treemap better??

    def examineFrontier(curr: CoordCost) = neighbors(curr).foreach { (newCost, newCoord) =>
      if (costSoFar.get(newCoord).filter(newCost>=_).isEmpty) { // add if not seen yet or new path is shorter    
        if (costSoFar.contains(newCoord)) frontier.mapInPlace(x => if (x._2==newCoord) (newCost, newCoord) else x) else frontier.enqueue((newCost, newCoord))
        costSoFar.update(newCoord, newCost)
        paths.update(newCoord, curr._2)
      }
    }

    while (!frontier.isEmpty && continue(paths)) examineFrontier(frontier.dequeue())
    AStarMap(origin, paths.toMap)
  }

  def path(grid: HexGrid[?, ?], origin: Coord, dest: Coord) = buildFrom(origin, neighborsCost1(grid), untilReach(dest)).pathTo(dest)
  def sparsePath(grid: HexGrid[?, ?], origin: Coord, dest: Coord) = buildFrom(origin, sparseNeighborsCost1(grid), untilReach(dest)).pathTo(dest)
}
