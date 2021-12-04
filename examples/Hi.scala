def fun(): Unit = {
    print("input something: ");
    val x = readLine();
    print("input one more: ");
    val y = readLine();
    val p = List.Cons(x, List.Cons(y, List.Nil()));
    println(head(p) ++ "\n" ++ toString(tail(p)))
}

def getList(): List[Int] = {
    print("Input a number ('x' to finish): ");
    val x = readLine();
    if (x == "x") {
        List.Nil()
    } else {
        val y: Int = toInt(x);
        List.Cons(y, getList())
    }
}

@main
def hi(): Unit = {
    val a = Maybe.Just(Maybe.Just(12));
    val b = Maybe.Just(Maybe.Nothing());
    println(toString(a == b));
    println(toString("hello" != "hell0"));
    println(toString(Maybe.Just(true)));
    fun();
    val l = getList();
    println(toString(l));
    println(toString(mergeSort(l)));
    println("double length is " ++ toString(length(append(l,l))));
    println("sum is " ++ toString(sum(l)))
}
