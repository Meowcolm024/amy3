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

def getList(): List[Int] = {
    print("Input a number ('x' to finish): ");
    val x: String = readLine();
    if (x == "x") {
        List.Nil()
    } else {
        val y: Int = parseInt(x);
        List.Cons(y, getList())
    }
}

@main
def hi(): Unit = {
    val a: Maybe[Maybe[Int]] = Maybe.Just(Maybe.Just(12));
    val b: Maybe[Maybe[Int]] = Maybe.Just(Maybe.Nothing());
    println(toString(a == b));
    println(toString("hello" != "hell0"));
    println(toString(Maybe.Just(true)));
    // omg()
    val l : List[Int] = getList();
    println(toString(l));
     println(toString(mergeSort(l)));
    println("double length is " ++ toString(length(append(l,l))));
    println("sum is " ++ toString(sum(l)))
}
