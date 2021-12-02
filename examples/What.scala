enum List[A] {
  case Nil()
  case Cons(head: A, tail: List[A])
}

def sum(l: List[Int]): Int = {
  l match {
    case List.Nil() => 0
    case List.Cons(v, ls) => v + sum(ls)
  }
}