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

def getOr[A](m: Maybe[A], opt: A): A = {
  m match {
    case Maybe.Nothing() => opt
    case Maybe.Just(n) => n
  }
}

def listToPair[A](l: List[A]): Maybe[Pair[A, List[A]]] = {
  l match {
    case List.Nil() => Maybe.Nothing[A]()
    case List.Cons(h, t) => Maybe.Just[A](Pair.Pair[A, List[A]](h, t))
  }
}

def safeDiv(x: Int, y: Int): Maybe[Int] = {
  if (!(y == 0)) {
    Maybe.Just[Int](x / y)
  } else {
    Maybe.Nothing[Int]()
  }
}

@main
def hello(): Unit = {
  val a: Maybe[Int] = Maybe.Just[Int](3);
  val b: Int = 2+3;
  val l: List[Boolean] = List.Cons[Boolean](true, List.Cons[Boolean](false, List.Nil[Boolean]()));
  println(safeDiv(getOr[Int](a, b), 0));
  println(-1 == (5 * 6) && !true);
  println(listToPair[Boolean](l))
}
