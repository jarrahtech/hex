package com.jarrahtechnology.hex

final case class Coord(x: Int, y: Int) {
    inline def column = x
    inline def row = y
    def +(that: Coord) = Coord(x+that.x, y+that.y)
    def -(that: Coord) = Coord(x-that.x, y-that.y)
    def unary_- = Coord(-x, -y)
}

object Coord {
    val zero = Coord(0, 0)
    val north = Coord(0, 1)
    val up = north
    val northEast = Coord(1, 1)
    val east = Coord(1, 0)
    val right = east
    val southEast = Coord(1, -1)
    val south = Coord(0, -1)
    val down = south
    val southWest = Coord(-1, -1)
    val west = Coord(-1, 0)
    val left = west
    val northWest = Coord(-1, 1)
}

final case class CubeCoord(q: Int, r: Int, s: Int) {
    def +(that: CubeCoord) = CubeCoord(q+that.q, r+that.r, s+that.s)
    def -(that: CubeCoord) = CubeCoord(q-that.q, r-that.r, s-that.s)
    def unary_- = CubeCoord(-q, -r, -s)
    def distance(to: CubeCoord) = math.max(math.abs(q - to.q), math.max(math.abs(r - to.r), math.abs(s - to.s)))
}

object CubeCoord {
    val zero = CubeCoord(0, 0, 0)

    def apply(q: Int, r: Int): CubeCoord = CubeCoord(q, r, -q-r)

    def round(x: Float, y: Float, z: Float) = {
        val rx = math.round(x)
        val ry = math.round(y)
        val rz = math.round(z)

        val x_diff = math.abs(rx - x)
        val y_diff = math.abs(ry - y)
        val z_diff = math.abs(rz - z)

        if (x_diff > y_diff && x_diff > z_diff) {
            CubeCoord(-ry-rz, ry, rz)
        } else if (y_diff > z_diff) {
            CubeCoord(rx, -rx-rz, rz)
        } else {
            CubeCoord(rx, ry, -rx-ry)
        }
    }
}