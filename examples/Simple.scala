enum Color {
  case Red()
  case Blue()
}

enum ColorList[A] {
  case ColorList(c: List[Color])
}

@main
def sim(): Unit = {
  val x = List.Cons("haha",List.Cons("hahahaha", List.Nil()));
  val y = List.Cons(Maybe.Just(2), List.Nil());
  println(toString(getOr(head(y), 3)) ++ head(x))
}
