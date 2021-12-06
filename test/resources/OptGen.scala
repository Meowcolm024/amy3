def omg(): Int = {
    1
}

def testLitFold(): Int = {
    val x = 1 + 2 * 3;
    val y = "hello" ++ " world";
    println(toString(x * 233 / 2));
    println(y ++ "!");
    x
}

@main
def test(): Unit = {
    testLitFold()
}