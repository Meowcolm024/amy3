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

def sum(l: List[Int]): Int = {
  l match {
    case List.Nil() => 0
    case List.Cons(v, ls) => v + sum(ls)
  }
}

@main
def haha(): Unit = {
  val l1: List[Int] = what(5);
  println(toString(l1));
  println(toString(!true || false));
  val s: Int = sum(l1);
  println(toString(s))
}