def fib(n: Int): Int = {
    n match {
        case 0 => 0
        case 1 => 1
        case _ => fib(n-1) + fib(n-2)
    }
}

def fact(m: Int): Int = {
    if (m == 0) {
        1
    } else {
        m * fact(m-1)
    }
}

def testFib(): Int = {
    fib(10)
}

def testFact(): Int = {
    fact(10)
}
