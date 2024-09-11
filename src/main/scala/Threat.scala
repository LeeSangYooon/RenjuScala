class Threat(val length: Int, val openPoints: Set[Int], val color: Int) {
  override def toString: String = f"length: ${length}   ${openPoints.map(n => f"${n % 15} ${n / 15} ")}"


}