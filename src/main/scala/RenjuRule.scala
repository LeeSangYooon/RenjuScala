import RenjuRule.{BLACK, EMPTY, WHITE}

import collection.mutable.Stack
import scala.collection.mutable

object RenjuRule {
  val EMPTY = 0
  val WHITE: Int = -1
  val BLACK = 1
  private var linesSet = Map[Int, Set[Line]]()
  private var linesForCoordinate: Map[Int, Array[Set[Line]]] = Map()
  def newGame(boardSize: Int): RenjuRule = RenjuRule(boardSize, Array.fill(boardSize * boardSize)(EMPTY), BLACK)
  def getLines(boardSize: Int): Set[Line] =
    if (linesSet.contains(boardSize))
      linesSet(boardSize)
    else {
      // 한 방향으로만 ..
      var lines = Set[Line]()
      // 가로 세로
      for (n <- 0 until boardSize) {
        lines += Line(boardSize, n, 0, 0, 1)
        lines += Line(boardSize, 0, n, 1, 0)
      }
      // 대각선
      if (boardSize >= 5) {
        lines += Line(boardSize, 0, 0, 1, 1)
        for (n <- 1 to boardSize - 5) {
          lines += Line(boardSize, n, 0, 1, 1)
          lines += Line(boardSize, 0, n, 1, 1)
        }

        lines += Line(boardSize, boardSize - 1, 0, -1, 1)
        for (n <- 1 to boardSize - 5) {
          lines += Line(boardSize, boardSize - 1 - n, 0, -1, 1)
          lines += Line(boardSize, boardSize - 1, n, -1, 1)
        }
      }

      linesSet += (boardSize -> lines)

      linesForCoordinate += (boardSize -> Array.fill(boardSize * boardSize)(Set()))
      for (line <- lines) {
        line.iterate((x,y,_,_) => {
          linesForCoordinate(boardSize)(x+y*boardSize) += line
        })
      }

      lines
    }
  def getLinesAt(size: Int)(p: Int): Set[Line] = linesForCoordinate(size)(p)

}

case class Line(boardSize: Int, x0: Int, y0: Int, dx: Int, dy: Int) {
  override def toString: String = f"x: ${x0} y: ${y0} dy/dx: ${dy}/${dx}  len: ${length}"
  val length: Int = {
    val xLength =
      if (dx == 0)
        boardSize + 1
      else if (dx == 1)
        boardSize - x0
      else
        x0 + 1
    val yLength =
      if (dy == 0)
        boardSize + 1
      else if (dy == 1)
        boardSize - y0
      else
        y0 + 1
    Math.min(xLength, yLength)
  }

  def getMember(i: Int) = (x0 + dx * i, y0 + dy * i)

  def getCellPositions: Seq[(Int, Int)] = (0 until length).map((n) => getMember(n))

  def iterate(func: (Int, Int, Int, Int)=>Unit, reversed: Boolean = false): Unit = {
    for (i <- if (reversed) (0 until length).reverse else 0 until length) {

      val next = if (reversed) i - 1 else i + 1
      var nextCell = -1
      if (0 <= next && next < length) {
        nextCell = getMember(next)._1 + getMember(next)._2 * boardSize

      func(getMember(i)._1, getMember(i)._2, i, nextCell)
      }
    }
  }
  def iterateBothDirection(func: (Int, Int, Int, Int)=>Unit, start: () => Unit = () => ()): Unit = {
    start()
    iterate(func)
    start()
    iterate(func, reversed = true)
  }
}


case class RenjuRule(size: Int, board: Array[Int], turn: Int, lastMove: Int = -1) {
  private val boardCells = size * size
  private val lines: Set[Line] = RenjuRule.getLines(size)

  val state: Int = {
    val linesToSee =
      if (lastMove == -1)
        lines
      else
        RenjuRule.getLinesAt(size)(lastMove)

    var winner = 0
    for (line <- linesToSee if winner == 0) {
      val stack = mutable.Stack[Int]()
      line.iterate((x,y,n,_) => {
        val cell = board(x + y*size)
        if (cell == EMPTY) {
          stack.clear()
        } else {
          if (stack.isEmpty) {
            stack.append(cell)
          } else{
            if (stack.top != cell) {
              stack.clear()
            }
            stack.append(cell)
            if (stack.length == 5)
              winner = cell
          }
        }
      })
    }
    winner
  }

  val availablePositions: Array[Boolean] = {
    if (turn == RenjuRule.WHITE)
      board.map(p => p == 0)
    else {
      availablePositionsForBlack
    }
  }

  private def availablePositionsForBlack: Array[Boolean] = {
    val wholeDeltaCountOf3 = Array.fill(boardCells)(0)
    val wholeDeltaCountOf4 = Array.fill(boardCells)(0)

    // 흑
    for (line <- lines) {
      val deltaCountOf3 = Array.fill(line.length)(0)
      val deltaCountOf4 = Array.fill(line.length)(0)

      val cellsInLine = line.getCellPositions.map({ case (x, y) => board(x + y * size) }).toList

      val originalBoardCountOf3 = getNumberOf3inArray(cellsInLine, RenjuRule.BLACK)
      val originalBoardCountOf4 = getNumberOf4inArray(cellsInLine, RenjuRule.BLACK)
      //println(line)
      line.iterate((x, y, n, _) => {
        val cell = board(x + y * size)
        if (cell == RenjuRule.EMPTY) {
          val supposeToBeBlack = n
          val t = getNumberOf3inArray(cellsInLine.updated(n, RenjuRule.BLACK), RenjuRule.BLACK)
          val f = getNumberOf4inArray(cellsInLine.updated(n, RenjuRule.BLACK), RenjuRule.BLACK)

          deltaCountOf3(supposeToBeBlack) += t - originalBoardCountOf3
          deltaCountOf4(supposeToBeBlack) += f - originalBoardCountOf4
          if (f != 0) {
            //println(deltaCountOf4(supposeToBeBlack))
            //println((x, y))
          }
        }
      })

      line.iterate((x, y, n, _) => {
        val index = x + y * size
        wholeDeltaCountOf3(index) += deltaCountOf3(n)
        wholeDeltaCountOf4(index) += deltaCountOf4(n)
      })
    }
    //((0 until boardCells).map(n => wholeDeltaCountOf4(n)).toArray.mkString("Array(", ", ", ")"))
    //println((0 until boardCells).map(n => wholeDeltaCountOf4(n) <= 1 && wholeDeltaCountOf3(n) <= 1).toArray.mkString("Array(", ", ", ")"))
    (0 until boardCells).map(n => board(n) == EMPTY & wholeDeltaCountOf4(n) <= 1 && wholeDeltaCountOf3(n) <= 1 && !doesItMake6(n)).toArray
  }

  def getAll4InBoard: Set[Int] = {
    val wholeDeltaCountOf4 = Array.fill(boardCells)(0)
    // 흑
    for (line <- lines) {
      val deltaCountOf4 = Array.fill(line.length)(0)

      val cellsInLine = line.getCellPositions.map({ case (x, y) => board(x + y * size) }).toList

      val originalBoardCountOf4 = getNumberOf4inArray(cellsInLine, turn)
      //println(line)
      line.iterate((x, y, n, _) => {
        val cell = board(x + y * size)
        if (cell == RenjuRule.EMPTY) {
          val supposeToBeBlack = n
          val f = getNumberOf4inArray(cellsInLine.updated(n, turn), turn)

          deltaCountOf4(supposeToBeBlack) += f - originalBoardCountOf4
        }
      })

      line.iterate((x, y, n, _) => {
        val index = x + y * size
        wholeDeltaCountOf4(index) += deltaCountOf4(n)
      })
    }
    (0 until boardCells).filter(n => availablePositions(n) && wholeDeltaCountOf4(n) >= 1).toSet
  }

  // returns number of 3 and 4
  private def getNumberOf3inArray(array: List[Int], color: Int): Int = {
    var count3 = 0
    var seqCount = 0
    var emptyCount = 0
    var startingPoint = 0
    for (i <- array.indices; arr = array(i)) {
      if (arr == color) {
        if (seqCount == 0) {
          startingPoint = i
        }
        seqCount += 1
      }
      else if(arr == -color) {
        seqCount = 0
        emptyCount = 0
      }
      else {
        // arr == RenjuRule.EMPTY
        if (seqCount == 3) {
          val left = startingPoint - 1
          val isLeftEmpty = left >= 0 && array(left) == RenjuRule.EMPTY
          val right = i
          val isRightEmpty = right < array.length && array(right) == RenjuRule.EMPTY
          if (isLeftEmpty && isRightEmpty) {
            val leftleft = startingPoint - 2
            val isLeftLeftEmpty = leftleft >= 0 && array(leftleft) == RenjuRule.EMPTY
            val rightright = i + 1
            val isRightRightEmpty = rightright < array.length && array(rightright) == RenjuRule.EMPTY
            if (isLeftLeftEmpty || isRightRightEmpty) {
              count3 += 1
            }
          }
          seqCount = 0
          emptyCount = 0
        }
        else {
          if (seqCount > 0 && emptyCount == 0) {
            emptyCount += 1
          }
          else {
            // 공배 두개면 열린 3 X
            seqCount = 0
            emptyCount = 0
          }
        }

      }
    }
    count3
  }

  private def getNumberOf4inArray(array: List[Int], color: Int): Int = {
    var count4 = 0
    var seqCount = 0
    var emptyCount = 0
    var startingPoint = 0
    for (i <- array.indices; arr = array(i)) {
      if (arr == color) {
        if (seqCount == 0) {
          startingPoint = i
        }
        seqCount += 1
      }
      else if (arr == -color) {
        if (seqCount == 4) {
          if (emptyCount == 1) {
            count4 +=1
          }
          else {
            val leftLeft = startingPoint -2
            if (!(leftLeft > 0 && array(leftLeft) == color)) {
              val left =startingPoint-1
              if (left >0 && array(left)==RenjuRule.EMPTY)
                count4 +=1
            }
          }
        }
        seqCount = 0
        emptyCount = 0
      }
      else {
        // arr == RenjuRule.EMPTY
        if (seqCount == 4) {
          if (emptyCount == 0) {
            val left = startingPoint - 1
            val isLeftEmpty = left >= 0 && array(left) == RenjuRule.EMPTY
            val right = i
            val isRightEmpty = right < array.length && array(right) == RenjuRule.EMPTY
            //println("뿌직")
            //println((left, right))
            //println((isLeftEmpty, isRightEmpty))
            if (isLeftEmpty || isRightEmpty) {
              count4 += 1
            }
          }
          else {
            count4 += 1
          }
          if (array(i - 2) == RenjuRule.EMPTY) {
            seqCount = 1
            emptyCount = 1
            startingPoint = i - 1
          }
          else {
            seqCount =0
            emptyCount = 1
          }
        }
        else {
          if (seqCount > 0 && emptyCount == 0) {
            emptyCount += 1
          }
          else {
            // 공배 두개면 열린 4 X
            seqCount = 0
            emptyCount = 0
          }
        }

      }
    }
    if (seqCount == 4) {
      if (startingPoint - 1 >0 && array(startingPoint - 1) == RenjuRule.EMPTY) {
        count4 += 1
      }
      else if(emptyCount == 1){
        count4 += 1
      }
    }

    count4
  }

  private def doesItMake6(p: Int): Boolean = {
    val (px, py) = (p % size, p / size)
    val lines = RenjuRule.getLinesAt(size)(p)
    for (line <- lines) {
      var is6 = false
      var seq = 0
      line.iterate((x,y,_,_) => {
        if (board(x+y*size) == turn || (x == px && y == py))
          seq += 1
        else
          seq = 0

        if (seq > 5)
          is6 = true
      })
      if (is6)
        return true
    }
    false
  }


  def getAll3threatInArray(array: List[Int], color: Int): Set[Threat] = {
    var set3 = Set[Threat]()
    var seqCount = 0
    var emptyCount = 0
    var startingPoint = 0
    for (i <- array.indices; arr = array(i)) {
      if (arr == color) {
        if (seqCount == 0) {
          startingPoint = i
        }
        seqCount += 1
      }
      else if (arr == -color) {
        seqCount = 0
        emptyCount = 0
      }
      else {
        // arr == RenjuRule.EMPTY
        if (seqCount == 3) {
          val left = startingPoint - 1
          val isLeftEmpty = left >= 0 && array(left) == RenjuRule.EMPTY
          val right = i
          val isRightEmpty = right < array.length && array(right) == RenjuRule.EMPTY
          if (isLeftEmpty && isRightEmpty) {
            val leftleft = startingPoint - 2
            val isLeftLeftEmpty = leftleft >= 0 && array(leftleft) == RenjuRule.EMPTY
            val rightright = i + 1
            val isRightRightEmpty = rightright < array.length && array(rightright) == RenjuRule.EMPTY
            if (isLeftLeftEmpty || isRightRightEmpty) {
              if (emptyCount == 1){
                var j = i - 1
                while (array(j) != RenjuRule.EMPTY)
                  j -= 1

                set3 += new Threat(3, Set(j, left, right), color)

              }
              else{
                set3 += new Threat(3, Set(left, right), color)
              }
            }
          }
          seqCount = 0
          emptyCount = 0
        }
        else {
          if (seqCount > 0 && emptyCount == 0) {
            emptyCount += 1
          }
          else {
            // 공배 두개면 열린 3 X
            seqCount = 0
            emptyCount = 0
          }
        }

      }
    }
    set3
  }
  def getAll4threatInArray(array: List[Int], color: Int): Set[Threat] = {
    var set4 = Set[Threat]()
    var seqCount = 0
    var emptyCount = 0
    var startingPoint = 0
    for (i <- array.indices; arr = array(i)) {
      if (arr == color) {
        if (seqCount == 0) {
          startingPoint = i
        }
        seqCount += 1
      }
      else if (arr == -color) {
        if (seqCount == 4) {
          if (emptyCount == 1) {
            var j = i - 1
            while (array(j) != RenjuRule.EMPTY)
              j -= 1
            set4 += new Threat(4, Set(j), color)
          }
          else {
            val leftLeft = startingPoint - 2
            if (!(leftLeft > 0 && array(leftLeft) == color)) {
              val left = startingPoint - 1
              if (left > 0 && array(left) == RenjuRule.EMPTY)
                set4 += new Threat(4, Set(left), color)
            }
          }
        }
        seqCount = 0
        emptyCount = 0
      }
      else {
        // arr == RenjuRule.EMPTY
        if (seqCount == 4) {
          if (emptyCount == 0) {
            val left = startingPoint - 1
            val isLeftEmpty = left >= 0 && array(left) == RenjuRule.EMPTY
            val right = i
            val isRightEmpty = right < array.length && array(right) == RenjuRule.EMPTY

            if (isLeftEmpty || isRightEmpty) {
              var s = Set[Int]()
              if (isLeftEmpty) s += left
              if (isRightEmpty) s += right
              set4 += new Threat(4, s, color)
            }
          }
          else {
            var j = i - 1
            while (array(j) != RenjuRule.EMPTY)
              j -= 1
            set4 += new Threat(4, Set(j), color)
          }
          if (array(i - 2) == RenjuRule.EMPTY) {
            seqCount = 1
            emptyCount = 1
            startingPoint = i - 1
          }
          else {
            seqCount = 0
            emptyCount = 1
          }
        }
        else {
          if (seqCount > 0 && emptyCount == 0) {
            emptyCount += 1
          }
          else {
            // 공배 두개면 열린 4 X
            seqCount = 0
            emptyCount = 0
          }
        }

      }
    }
    if (seqCount == 4) {
      if (startingPoint - 1 > 0 && array(startingPoint - 1) == RenjuRule.EMPTY) {
        set4 += new Threat(4, Set(startingPoint - 1), color)
      }
      else if (emptyCount == 1) {
        var j = array.length - 2
        while (array(j) != RenjuRule.EMPTY)
          j -= 1
        set4 += new Threat(4, Set(j), color)
      }
    }

    set4
  }

  def getAll3and4onBoard(color: Int): Set[Threat] = {
    lines.flatMap(line => {
      val cellsInLine = line.getCellPositions.map({ case (x, y) => board(x + y * size) }).toList
      // 1차원 좌표를 2차원 좌표로 바꿔야한다
      (getAll3threatInArray(cellsInLine, color) ++ getAll4threatInArray(cellsInLine, color))
        .map(t => new Threat(t.length, t.openPoints.map(p => {
          val positionIn2d = line.getMember(p)
          positionIn2d._1 + positionIn2d._2 * size
        }), color))
    })
  }


  def getAllAttacksInOrder(color: Int): Array[Int] = {
    val wholeDeltaCountOf3 = Array.fill(boardCells)(0)
    val wholeDeltaCountOf4 = Array.fill(boardCells)(0)

    // 흑
    for (line <- lines) {
      val deltaCountOf3 = Array.fill(line.length)(0)
      val deltaCountOf4 = Array.fill(line.length)(0)

      val cellsInLine = line.getCellPositions.map({ case (x, y) => board(x + y * size) }).toList

      val originalBoardCountOf3 = getNumberOf3inArray(cellsInLine, color)
      val originalBoardCountOf4 = getNumberOf4inArray(cellsInLine, color)
      //println(line)
      line.iterate((x, y, n, _) => {
        val cell = board(x + y * size)
        if (cell == RenjuRule.EMPTY) {
          val supposeToBeBlack = n
          val t = getNumberOf3inArray(cellsInLine.updated(n, color), color)
          val f = getNumberOf4inArray(cellsInLine.updated(n, color), color)

          deltaCountOf3(supposeToBeBlack) += t - originalBoardCountOf3
          deltaCountOf4(supposeToBeBlack) += f - originalBoardCountOf4
          if (f != 0) {
            //println(deltaCountOf4(supposeToBeBlack))
            //println((x, y))
          }
        }
      })

      line.iterate((x, y, n,_) => {
        val index = x + y * size
        wholeDeltaCountOf3(index) += deltaCountOf3(n)
        wholeDeltaCountOf4(index) += deltaCountOf4(n)
      })
    }

    val tupleSeq = (0 until boardCells).filter(n => availablePositions(n)).map(n => (n, wholeDeltaCountOf4(n) + wholeDeltaCountOf3(n)))
    tupleSeq.filter(t => t._2 != 0).sortBy(t => -t._2).map(t => t._1).toArray
  }

  private def weight(seqCount: Int, open: Boolean): Float = Math.sqrt(seqCount).toFloat * (if (open) 2f else 1)

  private def weightMap(color: Int = 0): Array[Float] = {
    val wholeSum = Array.fill(boardCells)(0f)
    for (line <- lines) {
      val weightSum = Array.fill(size)(0f)
      var currentSeqColor = 0
      var seqCount = 0
      var isBackOpen = false
      line.iterateBothDirection((x, y, n, next) => {
        val cell = board(x + y * size)

        if (cell == 0) {
          if (next != -1 && board(next) != -currentSeqColor) {
            if (color == 0 || color == seqCount)
              weightSum(n) += weight(seqCount, isBackOpen)
            else
              weightSum(n) -= weight(seqCount, isBackOpen)

          }
          isBackOpen = true
          seqCount = 0
          currentSeqColor = 0
        }
        else {
          if (currentSeqColor == 0 || cell == currentSeqColor) {
            currentSeqColor = cell
            seqCount += 1
          }

          if (currentSeqColor != cell) {
            seqCount = 0
            currentSeqColor = 0
            isBackOpen = false
          }
        }
      }, () => {
        currentSeqColor = 0
        seqCount = 0
        isBackOpen = false
      })
      line.iterate((x, y, n, _) => {
        val index = x + y * size
        wholeSum(index) += weightSum(n)
      })
    }
    wholeSum
  }
  def getGoodMovesInOrder(n: Int, color: Int): Array[Int] = {
    val wholeSum = weightMap(0)
    val sortedMoves = (0 until boardCells).filter(l => availablePositions(l)).sortBy(l => -wholeSum(l))
    //println(sortedMoves)
    sortedMoves.slice(0, Math.min(sortedMoves.length, n)).toArray
  }
  def eval(color: Int): Float = {
    weightMap(color).sum
  }

  def show(): Unit = {
    for (y <- 0 until size) {
      for (x <- 0 until size) {
        val c = board(x+y*size)
        print(if(c == WHITE) "W" else if (c == BLACK) "B" else "E")
      }
      println()
    }
    println()
  }
  def put(p: Int): RenjuRule = {
    assert(state == 0)
    assert(availablePositions(p))
    val newBoard = board.updated(p, turn)
    RenjuRule(size, newBoard, -turn, p)
  }
}
