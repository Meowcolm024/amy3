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

def and(l: List[Boolean]): Boolean = {
    l match {
        case List.Nil() => false
        case List.Cons(x, List.Nil()) => x
        case List.Cons(x, xs) => x && and(xs)
    }
}

def testSum(): Int = {
    val xs: List[Int] = List.Cons(1, List.Cons(2, List.Cons(3, List.Cons(4, List.Nil()))));
    sum(xs)
}

def testAnd(): Boolean = {
    val xs: List[Boolean] = List.Cons(true, List.Cons(true, List.Cons(true, List.Cons(false, List.Nil()))));
    and(xs)
}
