def matmat(i: Int): Int = {
  i match {
    case 1 => 1
    case _ => 2
  } match {
    case 2 => 1
    case _ => i
  }
}

@main
def sim(): Unit = {
  println(toString(matmat(2)))
}