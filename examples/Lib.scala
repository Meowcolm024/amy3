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
