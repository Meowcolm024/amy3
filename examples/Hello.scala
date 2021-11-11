enum Maybe[A] {
    case Nothing()
    case Just(n: A)
}

def getOr[A](m: Maybe[A], opt: A): A ={
    m match {
        case Maybe.Nothing() => opt
        case Maybe.Just(n) => n
    }
}

def safeDiv(x: Int, y: Int): Maybe[Int] = {
    if (!(y == 0)) {
        Maybe.Just(x/y)
    } else {
        Maybe.Nothing()
    }
}

@main
def hello(): Unit = {
    val a: Maybe[Int] = Maybe.Just[Int](3);
    print[Int](233)
}