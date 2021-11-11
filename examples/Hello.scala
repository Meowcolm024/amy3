enum Maybe[A] {
    case Nothing()
    case Just(n: A)
}

def getOr[A](m: Maybe[A], opt: A): A ={
    m match {
        case Maybe.Nothing[A]() => opt
        case Maybe.Just[A](n) => n
    }
}

def safeDiv(x: Int, y: Int): Maybe[Int] = {
    if (!(y == 0)) {
        Maybe.Just[Int](x/y)
    } else {
        Maybe.Nothing[Int]()
    }
}

@main
def hello(): Unit = {
    val a: Maybe[Int] = Maybe.Just[Int](3);
    print[Int](getOr[Int](a, 5));
    print(() + (5 * 6) && !true)
}