def omg(): Int = {
    1
}

def testLitFold(): Unit = {
    val x = 1 + 2 * 3;
    val y = "hello" ++ " world";
    println(toString(x * 233 / 2 + omg()));
    println(y ++ "!");
    val z = 1 + omg();
    print(toString(z / z + omg()))
}

@main
def test(): Unit = {
    testLitFold()
}