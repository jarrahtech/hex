package com.jarrahtechnology.hex

final case class Coord(x: Int, y: Int) {
    inline def column = x
    inline def row = y
    def +(that: Coord) = Coord(x+that.x, y+that.y)
    def -(that: Coord) = Coord(x-that.x, y-that.y)
    def unary_- = Coord(-x, -y)
}

object Coord {
    val zero = Coord(0,0)
}

final case class CubeCoord(x: Int, y: Int, z: Int) {
    def +(that: CubeCoord) = CubeCoord(x+that.x, y+that.y, z+that.z)
    def -(that: CubeCoord) = CubeCoord(x-that.x, y-that.y, z-that.z)
    def unary_- = CubeCoord(-x, -y, -z)
    def distance(to: CubeCoord) = math.max(math.abs(x - to.x), math.max(math.abs(y - to.y), math.abs(z - to.z)))
}

object CubeCoord {
    val zero = CubeCoord(0, 0, 0)

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