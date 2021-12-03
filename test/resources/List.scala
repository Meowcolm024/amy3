def and(l: List[Boolean]): Boolean = {
    l match {
        case List.Nil() => false
        case List.Cons(x, List.Nil()) => x
        case List.Cons(x, xs) => x && and(xs)
    }
}

// tests

def testSum(): Int = {
    val xs: List[Int] = List.Cons(1, List.Cons(2, List.Cons(3, List.Cons(4, List.Nil()))));
    sum(xs)
}

def testAnd(): Boolean = {
    val xs: List[Boolean] = List.Cons(true, List.Cons(true, List.Cons(true, List.Cons(false, List.Nil()))));
    and(xs)
}

def testSort(): List[Int] = {
  val xs: List[Int] = List.Cons(6, List.Cons(4, List.Cons(3, 
    List.Cons(0, List.Cons(9, List.Cons(2, List.Cons(7, List.Cons(1, List.Nil()))))))));
  mergeSort(xs)
}
