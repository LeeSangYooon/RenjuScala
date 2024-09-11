class Engine(renju: RenjuRule) {
  protected def directResponse: Option[Int] = {
    val opThreats = renju.getAll3and4onBoard(-renju.turn)
    val myThreats = renju.getAll3and4onBoard(renju.turn)

    if (myThreats.nonEmpty) {
      val myBiggestThreat = myThreats.maxBy(t => t.length)
      if (myBiggestThreat.length == 4)
        return Some(myBiggestThreat.openPoints.head)
    }
    if (opThreats.nonEmpty) {
      val opBiggestThreat = opThreats.maxBy(t => t.length)
      if (opBiggestThreat.length == 4)
        return Some(opBiggestThreat.openPoints.head)
    }

    if (myThreats.nonEmpty && (opThreats.isEmpty || opThreats.maxBy(t => t.length).length == 3)) {
      return Some(myThreats.head.openPoints.head)
    }

    None
  }

  def needToDefend: Boolean = {
    val opThreats = renju.getAll3and4onBoard(-renju.turn)
    val myThreats = renju.getAll3and4onBoard(renju.turn)
    if (myThreats.nonEmpty && myThreats.maxBy(t => t.length).length == 4)
      return false
    if (opThreats.isEmpty)
      return false
    if (myThreats.isEmpty)
      return true
    opThreats.maxBy(t => t.length).length > myThreats.maxBy(t => t.length).length
  }


  // Empty set == can not defend
  protected def defendMoves: Set[Int] = {
    val opThreats = renju.getAll3and4onBoard(-renju.turn)
    opThreats.flatMap(t => t.openPoints.filter(p => renju.availablePositions(p))) union (if(opThreats.maxBy(t=>t.length).length == 3) renju.getAll4InBoard else Set.empty)
  }

  protected def directWinningMove: Option[Int] = {
    val opThreats = renju.getAll3and4onBoard(-renju.turn)
    val myThreats = renju.getAll3and4onBoard(renju.turn)
    if (myThreats.nonEmpty) {
      val myBiggestThreat = myThreats.maxBy(t => t.length)
      if (myBiggestThreat.length == 4)
        return Some(myBiggestThreat.openPoints.head)
    }
    if (myThreats.nonEmpty && (opThreats.isEmpty || opThreats.maxBy(t => t.length).length == 3)) {
      return Some(myThreats.head.openPoints.head)
    }
    None
  }

  protected def searchForcedWin(depth: Int = 0): Option[Int] = {
    if (renju.state != 0) {
      assert (renju.state == -renju.turn)
      return Some(-10000)
    }

    if (needToDefend) return None

    directWinningMove match {
      case Some(n) => return Some(n)
      case None =>
    }

    if (depth == 0)
      return None

    val attacks = renju.getAllAttacksInOrder(renju.turn)
    //println(attacks.map(n => (n % 15, n / 15)).mkString("Array(", ", ", ")"))
    for (attack <- attacks) {
      val attackedGame = renju.put(attack)
      if (attackedGame.state != 0)
        return Some(attack)
      //attackedGame.show()
      val opEngine = new Engine(attackedGame)
      if (opEngine.needToDefend) {
        val defences = opEngine.defendMoves
        var isAllWinning = true
        if (defences.nonEmpty)
          for (defence <- defences if isAllWinning) {
            val defendedGame = attackedGame.put(defence)
            new Engine(defendedGame).searchForcedWin(depth - 1) match {
              case Some(n) =>
              case None => isAllWinning = false
            }
          }

        if (isAllWinning)
          return Some(attack)
      }
    }
    None
  }

  def goodMove(depth: Int = 3, candidates: Seq[Int] = Seq(), alpha: Int= -10000000, message: (Int) => Unit = (a: Int) => {}): (Int, Int) = {
    val moves = if(candidates.isEmpty) renju.getGoodMovesInOrder(6, renju.turn) else candidates.toArray
    if (depth == 0) {
      val attacked = renju.put(moves.head)
      val opEngine = new Engine(attacked)
      opEngine.searchForcedWin(3) match {
        case Some(value) =>
          return (moves.head, -10000)
        case None =>
          return (moves.head, (renju.eval(renju.turn)* 10).toInt)

      }
    }
    var best = -1000000
    var bestMove = moves.head

    message(bestMove)

    for (move <- moves) {

      val attacked = renju.put(move)
      var eval: Int = 0
      val opEngine = new Engine(attacked)
      opEngine.searchForcedWin(3) match {
        case Some(value) => eval = -10000
        case None => {
          var opCand = Seq[Int]()
          if (opEngine.needToDefend)
            opCand = opEngine.defendMoves.toSeq
          eval = -opEngine.goodMove(depth -1, opCand, bestMove)._2
        }


      }
      if(depth == 3) {
        message(bestMove)

      }
      if (alpha > -eval) {
        return (-1, eval)
      }
      if (eval > 9999) {
        return (move, eval)
      }
      if (eval > best){
        best = eval
        bestMove = move
      }
    }
    (bestMove, best)
  }


  def bestMove(m: (Int) => Unit = (a: Int) => {}): Int = {
    if (renju.board.count(n => n!=0) == 0)
      return renju.size * renju.size / 2
    if (needToDefend)  {
      if (defendMoves.isEmpty)
        return renju.getGoodMovesInOrder(1,renju.turn).head
      if (defendMoves.size == 1)
        return defendMoves.head

      val (move, eval) =goodMove(candidates = defendMoves.toSeq,  message = m)
        println(eval)

      return move
    }
    searchForcedWin(4) match {
      case Some(n) => n
      case None =>
        val (move, eval) = goodMove(candidates = renju.getGoodMovesInOrder(6, renju.turn), message = m)
        println(eval)
        move

    }
  }
}
