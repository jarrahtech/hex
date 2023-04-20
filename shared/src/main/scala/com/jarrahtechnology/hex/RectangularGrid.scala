package com.jarrahtechnology.hex

trait RectangularGrid(colRange: (Int, Int), rowRange: (Int, Int)) {
  val numColumns: Int = colRange._2 - colRange._1 + 1
  val numRows: Int = rowRange._2 - rowRange._1 + 1
  val capacity: Int = numColumns * numRows

  require(colRange._1 <= colRange._2, s"column range reversed: ${colRange._1}>${colRange._2}")
  require(rowRange._1 <= rowRange._2, s"row range reversed: ${rowRange._1}>${rowRange._2}")

  def inBounds(pos: Coord): Boolean = pos.column >= colRange._1 && pos.column <= colRange._2 && pos.row >= rowRange._1 && pos.row <= rowRange._2
}
