package dragon

import java.awt.{BasicStroke, Color}

import dragon.Edge.Move

object Config {
    val edgeLength = 10
    val generations = 15
    val startPosition = Pos(Image.width * 3 / 4, Image.height * 1 / 3)

    val fileName: String = "D:\\tmp\\drawing.png"

    object Image {
        val width: Int = 10000
        val height: Int = 10000
        val penColor: Color = Color.CYAN
        val lineStyle: BasicStroke = new BasicStroke()
    }

}

case class Line(start: Pos, end: Pos) {

    import java.lang.Math._

    def length: Double = sqrt(pow(end.x - start.x, 2) + pow(end.y - start.y, 2))
    def versor: Pos = (end - start).norm
}

object Main extends App {

    import dragon.Config._

    private val startLine: Line = Line(startPosition, startPosition + Pos(0, edgeLength))

    withTime("Drawing Dragon took: ") {

        val dragonCurve: Dragon = new Dragon(Config.generations)
        val lines: Seq[Line] = dragonCurve.edges.foldLeft(List(startLine))((p, e) => MoveToLine(p.head, e) :: p)

        Painter.lines(lines.reverse)
        Painter.out(fileName)

    }

    def MoveToLine(line: Line, move: Move): Line = {
        val direction = Direction(line.versor)
        move match {
            case Edge.Right => Line(line.end, direction.right.forward(line.end, line.length.toInt))
            case Edge.Left => Line(line.end, direction.left.forward(line.end, line.length.toInt))
        }
    }

    def withTime[B](msg: String)(f: => B): B = {
        val startTime: Long = System.nanoTime()

        val result = f

        val finishTime: Double = (System.nanoTime() - startTime) / 1000000.0
        println(s"$msg ${finishTime}ms")
        result
    }
}

object Painter {

    import java.awt.geom._
    import java.awt.image.BufferedImage
    import java.io.File
    import javax.imageio.ImageIO


    val imageSize = (Config.Image.width, Config.Image.height)
    val canvas = new BufferedImage(imageSize._1, imageSize._2, BufferedImage.TYPE_INT_RGB)
    val g = canvas.createGraphics()
    g.setStroke(Config.Image.lineStyle)
    g.setColor(Config.Image.penColor)

    def line(start: Pos, end: Pos) = g.draw(new Line2D.Double(start.x, start.y, end.x, end.y))

    def lines(l: Iterable[Line]) = l.par.foreach(elem => line(elem.start, elem.end))

    def out(fileName: String) = {
        ImageIO.write(canvas, "png", new File(fileName))
    }
}

object Edge {

    trait Move {
        def invert: Move
    }

    case object Right extends Move {
        def invert: Move = Left
    }

    case object Left extends Move {
        def invert: Move = Right
    }

}

class Dragon(generations: Int) {

    type Curve = Seq[Move]
    val startTurn: Curve = Seq(Edge.Right)

    lazy val edges: Curve = iteration(generations)

    def iteration(n: Int): Curve = {
        def iterAcc(n: Int, acc: Curve): Curve =
            if (n == 0) acc
            else iterAcc(n - 1, acc ++ startTurn ++ acc.reverse.map(_.invert))

        iterAcc(n, startTurn)
    }

}
