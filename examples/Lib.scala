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

def append[A](l: List[A], r: List[A]): List[A] = {
  l match {
    case List.Nil() => r
    case List.Cons(x, xs) => List.Cons(x, append(xs, r))
  }
}
