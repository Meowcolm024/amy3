@main
def sim(): Unit = {
  val x: Int = 
    if (true) {
      4 * 6
    } else {
      (3 + 4) * 0
    };
  println(toString((1 + 1) > x || true))
}