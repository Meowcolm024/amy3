enum Maybe[A] {
  case Nothing()
  case Just(n: A)
}

enum List[A] {
  case Nil()
  case Cons(head: A, tail: List[A])
}

enum Pair[A, B] {
  case Pair(x: A, y: B)
}

def head[A](l: List[A]): A = {
  l match {
    case List.Nil() => error("Empty list")
    case List.Cons(x, _) => x
  }
}

def tail[A](l: List[A]): List[A] = {
  l match {
    case List.Nil() => error("Empty list")
    case List.Cons(_, xs) => xs
  }
}

def length[A](l: List[A]): Int = {
  l match {
    case List.Nil() => 0
    case List.Cons(_, ls) => 1 + length(ls)
  }
}

def append[A](l: List[A], r: List[A]): List[A] = {
  l match {
    case List.Nil() => r
    case List.Cons(x, xs) => List.Cons(x, append(xs, r))
  }
}

def sum(l: List[Int]): Int = {
  l match {
    case List.Nil() => 0
    case List.Cons(v, ls) => v + sum(ls)
  }
}

def take[A](l: List[A], i: Int): List[A] = {
  l match {
    case List.Nil() => List.Nil()
    case List.Cons(x,xs) =>
      if (i == 0) {
        List.Nil()
      } else {
        List.Cons(x, take(xs, i-1))
      }
  }
}

def drop[A](l: List[A], i: Int): List[A] = {
  l match {
    case List.Nil() => List.Nil()
    case List.Cons(x,xs) =>
      if (i == 0) {
        l
      } else {
        drop(xs, i-1)
      }
  }
}

def split[A](l: List[A], i: Int): Pair[List[A], List[A]] = {
  Pair.Pair(take(l, i), drop(l, i))
}

def merge(p: List[Int], q: List[Int]): List[Int] = {
  Pair.Pair(p, q) match {
    case Pair.Pair(List.Nil(), ys) => ys
    case Pair.Pair(xs, List.Nil()) => xs
    case Pair.Pair(List.Cons(x, xs), List.Cons(y,ys)) => 
      if (x < y) {
        List.Cons(x, merge(xs, List.Cons(y,ys)))
      } else {
        List.Cons(y, merge(List.Cons(x, xs), ys))
      }
  }
}

def mergeSort(l: List[Int]): List[Int] = {
  l match {
    case List.Nil() => List.Nil()
    case List.Cons(x, List.Nil()) => l
    case _ => split(l, length(l)/2) match {
      case Pair.Pair(x, y) => merge(mergeSort(x), mergeSort(y))
    }
  }
}
