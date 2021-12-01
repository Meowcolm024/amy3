@main
def hi(): Unit = {
    print("input something: ");
    val x: String = readLine();
    val y: Int = 2 * parseInt(x);
    println(intToString(y*y))
}
