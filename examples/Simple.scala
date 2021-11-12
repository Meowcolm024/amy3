enum Maybe[A] {
  case Nothing()
  case Just(n: A)
}

enum Either[A, B] {
  case Left(l: A)
  case Right(r: B)
}

enum List[A] {
  case Nil()
  case Cons(head: A, tail: List[A])
}

enum LenList[A] {
  case LenList(len: Int, lst: List[A])
}
