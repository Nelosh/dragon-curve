package dragon

import scala.collection.mutable

object LocalImplicits {

    implicit class IntToPos(i: Int) {
        def *(p: Pos) = p * i
    }

}

case class Pos(x: Int, y: Int) {
    def +(that: Pos): Pos = Pos(this.x + that.x, this.y + that.y)
    def -(that: Pos): Pos = Pos(this.x - that.x, this.y - that.y)
    def *(n: Int): Pos = Pos(x * n, y * n)
    def /(n: Int): Pos = Pos(x / n, y / n)
    def norm: Pos = Pos(norm(x), norm(y))
    def norm(n: Int) = if (n > 0) 1 else if (n < 0) -1 else 0

}

case class Line(start: Pos, end: Pos) {

    import java.lang.Math._

    def length: Double = sqrt(pow(end.x - start.x, 2) + pow(end.y - start.y, 2))
    def versor: Pos = (end - start).norm
}

abstract class Direction(val versor: Pos) {
    def forward(start: Pos, n: Int): Pos = start + versor * n
    def right: Direction
    def left: Direction
    
}

object Direction {
    private val values: mutable.Map[Pos, Direction] = mutable.Map().withDefaultValue(Up)

    values(Up.versor) = Up
    values(Down.versor) = Down
    values(Right.versor) = Right
    values(Left.versor) = Left

    def apply(versor: Pos) = values(versor)
}

case object Up extends Direction(Pos(0, 1)) {
    def right = Right
    def left = Left
}

case object Down extends Direction(Pos(0, -1)) {
    def right = Left
    def left = Right
}

case object Left extends Direction(Pos(-1, 0)) {
    def right = Up
    def left = Down
}

case object Right extends Direction(Pos(1, 0)) {
    def right = Down
    def left = Up
}
