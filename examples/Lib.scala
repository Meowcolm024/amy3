enum Maybe[A] {
  case Nothing()
  case Just(n: A)
}

enum List[A] {
  case Nil()
  case Cons(head: A, tail: List[A])
}

enum Pair[A, B] {
  case Pair(x: A, y: B)
}

def length[A](l: List[A]): Int = {
  l match {
    case List.Nil() => 0
    case List.Cons(_, ls) => 1 + length(ls)
  }
}

def sum(l: List[Int]): Int = {
  l match {
    case List.Nil() => 0
    case List.Cons(v, ls) => v + sum(ls)
  }
}