def adds(): Int = {
    val x: Int = (1 + 2) + (3 + 4);
    x * (4 * 5)
}

def ret(x: Int): Int = {
    1 + 6 / 3 - x
}

def mix(): Int = {
    val p: Int = ret() * 10 / 5
    val q: Int = ret(x) + adds()
    p + ret() + q
}

def bools(): Boolean = {
    val x: Boolean = (1 > 2) || (1 < 2);
    val y: Boolean = !true && !false;
    (0 <= 2) || x || y
}
