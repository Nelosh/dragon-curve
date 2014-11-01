package dragon

object LocalImplicits {

  implicit class IntToPos(i: Int) {
    def *(p: Pos) = p * i
  }

}

case class Pos(x: Int, y: Int) {
  def +(that: Pos): Pos = Pos(this.x + that.x, this.y + that.y)
  def *(n: Int): Pos = Pos(this.x * n, this.y * n)
}

trait Direction {

  val versor: Pos

  def forward(start: Pos, n: Int): Pos = start + versor * n
  def right: Direction
  def left: Direction
}

case object Up extends Direction {
  val versor: Pos = Pos(0, 1)

  def right = Right
  def left = Left
}

case object Down extends Direction {
  val versor: Pos = Pos(0, -1)

  def right = Left
  def left = Right
}

case object Left extends Direction {
  val versor: Pos = Pos(-1, 0)

  def right = Up
  def left = Down
}

case object Right extends Direction {
  val versor: Pos = Pos(1, 0)

  def right = Down
  def left = Up
}

