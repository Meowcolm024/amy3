def omg(): Unit = {
    print("input something: ");
    val x: String = readLine();
    val y: Int = 2 * parseInt(x);
    println(toString(y*y));
    if (parseInt(x) != 0) {
        omg()
    } else {
        println("bye")
    }
}

@main
def hi(): Unit = {
    val a: Maybe[Maybe[Int]] = Maybe.Just(Maybe.Just(12));
    val b: Maybe[Maybe[Int]] = Maybe.Just(Maybe.Nothing());
    println(toString(a == b));
    println(toString("hello" != "hell0"));
    println(toString(Maybe.Just(true)));
    omg()
}
