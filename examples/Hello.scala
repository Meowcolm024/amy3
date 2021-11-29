enum Maybe[A] {
  case Nothing()
  case Just(n: A)
}

enum Color {
  case Red()
  case Green()
  case Blue()
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
    case List.Nil() => Maybe.Nothing()
    case List.Cons(h, t) => Maybe.Just(Pair.Pair(h, t))
  }
}

def swap[A, B](p: Pair[A, B]): Pair[B, A] = {
  p match {
    case Pair.Pair(x, y) => Pair.Pair(y, x)
  }
}

def safeDiv(x: Int, y: Int): Maybe[Int] = {
  if (!(y == 0)) {
    Maybe.Just(x / y)
  } else {
    Maybe.Nothing()
  }
}

@main
def hello(): Unit = {
  val a: Maybe[Int] = Maybe.Just(3);
  val b: Int = 2+3;
  val l: List[Boolean] = List.Cons(true, List.Cons(false, List.Nil()));
  val cp: Pair[Color, Maybe[Color]] = Pair.Pair(Color.Red(), Maybe.Just(Color.Blue()));
  println(safeDiv(getOr(a, b), 0));
  val b: Boolean = true || false;
  println(-1 == (5 * 6) && !b);
  println(listToPair(l))
}
