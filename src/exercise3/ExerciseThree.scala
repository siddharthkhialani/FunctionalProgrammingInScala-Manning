package exercise3

object ExerciseThree {

  //Ex 3.1 expected ans 3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4,_)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  //Ex 3.2
  def tail[A](list: List[A]) = {
    list match {
      case Nil => throw new Exception("Empty List")
      case Cons(_, xs) => xs
    }
  }

  // Ex 3.3
  def setHead[A](list: List[A], newHead: A) = {
    list match {
      case Nil => throw new Exception("Empty List")
      case Cons(_, xs) => Cons(newHead, xs)
    }
  }

  // Ex 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0)  l
    else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n-1)
    }
  }

  // Ex 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, as) => if (f(a)) dropWhile(as, f) else l
    }
  }

  // Ex 3.6
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(a, Nil) => Nil
      case Cons(a, as) => Cons(a, init(as))
    }
  }

  /* Ex 3.7 Answer No * requires both arguments. So even if left argument is 0.0
    it will not halt evaluating the subexpression on the right
  */

  // Ex 3.8

  //Ex 3.9
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z) (f))
    }

  def length[A] (as: List[A]) : Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  // Ex 3.10

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(y ,ys) => foldLeft(ys, f(z, y))(f)
    }
  }

  // Ex 3.11
  def sumFoldLeft( ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def productFoldLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def lengthFoldLeft[A] (as: List[A]) : Int = {
    foldLeft(as, 0)((y, _) => 1 + y)
  }

  // Ex 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((xs, y) => Cons(y, xs))
  }

  // Ex 3.13
  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b: B) => b) ((b, a) => s => b(f(a, s)) ) (z)
  }
  // Ex 3.14
  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRightUsingFoldLeft(a1, a2) ((x, ys) => Cons(x, ys))
  }

  // Ex 3.15
  def listOfLists[A](as: List[List[A]]): List[A] = {
    foldLeft(as, Nil: List[A]) ((b, a) => appendFoldRight(a, b))
  }

  def main(args: Array[String]) = {
    val listInt = List(1, 2, 3, 4, 5)
    val listDouble = List(1.0, 2.0, 3.0, 4.0, 5.0)
    println("3.1 " + x)
    println("3.2 " + tail(listInt))
    println("3.3 " + setHead(listInt, 10))
    println("3.4 " + drop(listInt, 2))
    println("3.5 " + dropWhile(listInt, (a: Int) => a == 1))
    println("3.6 " + init(listInt))

    println("3.9 " + length(listInt))
    println("3.10 " + foldLeft(listInt, 0) (_ + _))
    println("3.11 Sum = " + sumFoldLeft(listInt))
    println("3.11 Product = " + productFoldLeft(listDouble))
    println("3.11 Length = " + length(listInt))
    println("3.12 Reverse = " + reverse(listInt))
    println("3.14 " + appendFoldRight(List(1,2,3), List(4,5,6)))

  }
}
