package dragon

import java.awt.{BasicStroke, Color}

object Config {
    val edgeLength = 2
    val generations = 20
    val startPosition = Pos(Image.width * 3 / 4, Image.height * 3 / 4)

    val fileName: String = "D:\\tmp\\dragon-turtle.png"

    object Image {
        val width: Int = 10000
        val height: Int = 10000
        val penColor: Color = Color.CYAN
        val lineStyle: BasicStroke = new BasicStroke()
    }

}

object Main extends App {
    private val startTime: Long = System.nanoTime
    Dragon.draw(Config.generations)
    println((System.nanoTime() - startTime) / 1000000.0)
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

    def out(fileName: String) = {
        ImageIO.write(canvas, "png", new File(fileName))
    }
}

abstract class Turtle(var currentPos: Pos = Pos(0, 0)) {
    val painter = Painter

    var currentDirection: Direction = Up

    def right() = currentDirection = currentDirection.right
    def left() = currentDirection = currentDirection.left

    def forward(n: Int) = {
        val newPosition: Pos = currentDirection.forward(currentPos, n)
        drawLine(currentPos, newPosition)
        currentPos = newPosition
    }

    def drawLine(start: Pos, end: Pos) = painter.line(start, end)
    def saveImage() = painter.out(Config.fileName)
}


object Dragon {
    type Curve = Seq[Move]
    val startTurn: Curve = Seq(Right)

    object turtle extends Turtle(Config.startPosition)

    trait Move {

        def turn()
        def draw() = {
            turn(); turtle.forward(Config.edgeLength)
        }
        def invert: Move
    }

    case object Right extends Move {
        def turn() = turtle.right()
        def invert: Move = Left
    }

    case object Left extends Move {
        def turn() = turtle.left()
        def invert: Move = Right
    }


    def iteration(n: Int): Curve = {
        def iterAcc(n: Int, acc: Curve): Curve =
            if (n == 0) acc
            else iterAcc(n - 1, acc ++ startTurn ++ acc.reverse.map(_.invert))

        iterAcc(n, startTurn)
    }

    def draw(generations: Int) {
        iteration(generations) foreach (_.draw())
        turtle.saveImage()
    }
}
