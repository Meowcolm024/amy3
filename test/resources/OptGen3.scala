@main
def test(): Unit = {
    val x = readLine();
    val u = "zzzzz";
    val y = readLine();
    val z = x ++ " world";
    println("goodbye " ++ z);
    val t = readLine() ++ "hahaha";
    println(t ++ t ++ u);
    error("bye")
}