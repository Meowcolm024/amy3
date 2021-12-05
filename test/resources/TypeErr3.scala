enum Option[A] {
    case None()
    case Some(x: A)
}

def err(): Option[Int] = {
    val y = Option.None();
    val x: Option[Option[Int]] = Option.Some(10);
    y
}