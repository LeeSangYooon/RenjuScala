

object Main{
  def main(args: Array[String]): Unit = {
    val renju = RenjuRule.newGame(19)
    renju.put(10)
    println("Hello world!")
  }
}
