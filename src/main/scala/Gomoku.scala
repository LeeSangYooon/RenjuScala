import scala.swing._
import scala.swing.event._
import java.awt.{Color, Graphics2D, Image}
import java.io.File
import javax.imageio.ImageIO
import scala.concurrent._
import ExecutionContext.Implicits.global


object Gomoku extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "Go Board"
    preferredSize = new Dimension(1000, 650)

    val cellSize: Int = 40
    var renju: RenjuRule = RenjuRule.newGame(15)
    val boardSize: Int = renju.size
    var bestLookingMove: Int = -1

    var lastPlayed:(Int, Int) = (-1,0)

    val stones: Array[Array[Option[Color]]] = Array.fill(boardSize, boardSize)(Option.empty[Color])
    var boardImage: Option[Image] = None

    var renjuHistroy: Seq[RenjuRule] = List(renju)
    var engineThinking = false


    // Load the image
    try {
      boardImage = Some(ImageIO.read(new File("src/main/resources/board_background.jpg")))
    } catch {
      case ex: Exception => println(s"Error loading image: ${ex.getMessage}")
    }
    val progressBar: ProgressBar = new ProgressBar {
      min = 0
      max = 100
    }

    val boardPanel: Panel = new Panel {
      override def paintComponent(g: Graphics2D): Unit = {
        super.paintComponent(g)
        drawBackground(g)
        drawGrid(g)
        drawStones(g)
      }

      def drawBackground(g: Graphics2D): Unit = {
        boardImage.foreach(image => g.drawImage(image, 0, 0, size.width, size.height, null))
      }

      def drawGrid(g: Graphics2D): Unit = {
        g.setColor(Color.BLACK)
        for (i <- 0 until boardSize) {
          g.drawLine(cellSize / 2, cellSize / 2 + i * cellSize, cellSize / 2 + (boardSize - 1) * cellSize, cellSize / 2 + i * cellSize)
          g.drawLine(cellSize / 2 + i * cellSize, cellSize / 2, cellSize / 2 + i * cellSize, cellSize / 2 + (boardSize - 1) * cellSize)
        }
      }

      def drawStones(g: Graphics2D): Unit = {
        for (x <- 0 until boardSize; y <- 0 until boardSize) {
          if (renju.board(x + y * boardSize) == 0) {
            stones(x)(y) = None
          } else if (renju.board(x + y * boardSize) == 1){
            stones(x)(y) = Some(Color.BLACK)
          } else
            stones(x)(y) = Some(Color.WHITE)

        }
        for (x <- 0 until boardSize; y <- 0 until boardSize) {
          stones(x)(y) match {
            case Some(color) =>
              g.setColor(color)
              g.fillOval(x * cellSize, y * cellSize, cellSize, cellSize)
              g.setColor(Color.BLACK)
              g.drawOval(x * cellSize, y * cellSize, cellSize, cellSize)
            case None => if (!renju.availablePositions(x + y * boardSize)) {
              g.setColor(Color.RED)
              g.fillOval(((x + 0.5) * cellSize).toInt-7, ((y + 0.5) * cellSize).toInt-7, 14, 14)
            }
          }
          if (lastPlayed == (x, y)) {
            g.setColor(Color.ORANGE)
            g.fillOval(((x + 0.5) * cellSize).toInt - 7, ((y + 0.5) * cellSize).toInt - 7, 14, 14)
          }
          if (bestLookingMove == x + y *  boardSize) {
            g.setColor(Color.BLUE)
            g.drawOval(x * cellSize, y * cellSize, cellSize, cellSize)
          }
        }
      }
    }

    // Chat components
    val chatArea = new TextArea {
      editable = false
      lineWrap = true
    }

    // Top menu with a button
    val UndoItem = new MenuItem(Action("Undo") {
      renjuHistroy = renjuHistroy.dropRight(1)
      renju = renjuHistroy.last
      repaint()
    })

    override val menuBar: MenuBar = new MenuBar {
      contents += new Menu("Options") {
        contents += UndoItem
      }
    }

    val chatInput = new TextField

    val chatPanel: BorderPanel = new BorderPanel {
      layout(new ScrollPane(chatArea)) = BorderPanel.Position.Center
      layout(chatInput) = BorderPanel.Position.South
    }
    val splitPane: SplitPane = new SplitPane(Orientation.Vertical) {
      leftComponent = new BorderPanel {
        layout(boardPanel) = BorderPanel.Position.Center
        layout(progressBar) = BorderPanel.Position.South
      }
      rightComponent = chatPanel
      dividerLocation = 600
    }

    contents = new BorderPanel {
      layout(menuBar) = BorderPanel.Position.North
      layout(splitPane) = BorderPanel.Position.Center
    }

    listenTo(chatInput.keys)

    reactions += {
      case KeyPressed(_, Key.Enter, _, _) =>
        val message = chatInput.text.trim
        if (message.nonEmpty) {
          chatArea.append(s"User: $message\n")
          chatInput.text = ""
        }
    }


    listenTo(boardPanel.mouse.clicks, boardPanel.keys)

    var currentColor: Color = Color.BLACK

    private def enginePlay(): Unit = {
      boardPanel.repaint()

      progressBar.value =  0
      val engineMove = new Engine(renju).bestMove((m: Int) => {
        bestLookingMove = m
        progressBar.value += 100 / 6
        if (progressBar.value > 95)
          bestLookingMove = -1
        repaint()
      })
      renju = renju.put(engineMove)
      stones(engineMove % boardSize)(engineMove / boardSize) = Some(currentColor)
      currentColor = if (currentColor == Color.BLACK) Color.WHITE else Color.BLACK

      if (renju.state != 0) {
        println(f"${if (renju.state == 1) "흑" else "백"} 승")
      }
      lastPlayed = (engineMove % boardSize, engineMove / boardSize)
      boardPanel.repaint()
      boardPanel.requestFocus()
      renjuHistroy = renjuHistroy.appended(renju)

      engineThinking  =false
    }

    reactions += {
      case MousePressed(_, point, _, _, _) =>
        if (!engineThinking){
          val x = point.x / cellSize
          val y = point.y / cellSize
          if (x < boardSize && y < boardSize && stones(x)(y).isEmpty && renju.availablePositions(x + y * boardSize)) {
            stones(x)(y) = Some(currentColor)
            renju = renju.put(x + y * renju.size)
            currentColor = if (currentColor == Color.BLACK) Color.WHITE else Color.BLACK
            if (renju.state != 0) {
              println(f"${if (renju.state == 1) "흑" else "백"} 승")
            }
            lastPlayed = (x, y)
            boardPanel.repaint()
          }
          boardPanel.requestFocus()
          engineThinking = true
          Future {
            enginePlay()
          }
        }


      case KeyPressed(_, Key.A, _, _) =>
        enginePlay()

      case _ =>
        boardPanel.requestFocus()

    }
  }
}
