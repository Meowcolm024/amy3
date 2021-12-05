enum Box[A] {
    case Box(b: A)
}

def err(i: Box[String]): Box[Int] = {
   val x: Box[Int, Boolean] = Box.Box(10);
   x
}