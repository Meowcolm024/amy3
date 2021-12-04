def read(): Int = {
    val x: String = readLine();
    val y: String = readLine();
    toInt(x) + toInt(y)
}

@main
def doit(): Unit = {
    val r: Int = read();
    val t: Unit = r match {
        case 0 => println("is zero")
        case _ => println("not zero")
    };
    error("bye")
}
