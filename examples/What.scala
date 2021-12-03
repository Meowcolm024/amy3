enum List[A] {
  case Nil()
  case Cons(head: A, tail: List[A])
}

def what(i: Int): List[Int] = {
  if (i <= 0) {
    List.Nil()
  } else {
    List.Cons(i, what(i-1))
  }
}

@main
def haha(): Unit = {
  val l1: List[Int] = what(5);
  println(toString(l1));
  println(toString(!true || false));
  error("omg!")
}