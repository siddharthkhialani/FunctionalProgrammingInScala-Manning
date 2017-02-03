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

  // Ex 3.16
  def addOne(as: List[Int]): List[Int] = {
    as match {
      case Nil => Nil
      case Cons(y, ys) => Cons(y+1, addOne(ys))
    }
  }

  //Ex 3.17
  def doubleToString(as: List[Double]): List[String] = {
    as match {
      case Nil => Nil
      case Cons(y, ys) => Cons(y.toString, doubleToString(ys))
    }
  }

  // Ex 3.18
  def map[A, B](as: List[A]) (f: A => B): List[B] = {
   as match {
     case Nil => Nil
     case Cons(y, ys) => Cons(f(y), map(ys)(f))
   }
  }

  def mapUsingFoldLeft[A, B](as: List[A]) (f: A => B): List[B] = {
    foldLeft(as, (b:List[B]) => b) ((b, a) => s => b(Cons(f(a), s))) (Nil)
  }

  // Ex 3.19
  def filter[A] (as: List[A]) (f: A => Boolean): List[A] = {
    foldLeft(as, (b:List[A]) => b)((b, a) => if (f(a)) s => b(Cons(a, s)) else b) (Nil)
  }

  // Ex 3.20
  def flatMap[A, B] (as: List[A]) (f: A => List[B]) : List[B] = {
    foldLeft(as, (b:List[B]) => b)((b, a) => s => b(appendFoldRight(f(a), s))) (Nil)
  }

  // Ex 3.21
  def filterUsingFlatMap[A] (as: List[A]) (f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a))List(a) else Nil)
  }

  // Ex 3.34
  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
    def take[A](xs: List[A], n: Int): List[A] = {
      xs match {
        case Nil => Nil
        case Cons(a, as) => if (n == 0) Nil else Cons(a, take(as, n -1))
      }
    }
    foldLeft(as, (as, false)) ((b, a) => (drop(b._1, 1), (take(b._1, length(sub)) == sub) || b._2 ))._2
  }

  // Ex 3.25
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(b1, b2) => 1 + size(b1) + size(b2)
      case _ => 0
    }
  }
  // Ex 3.26
   def maximum(tree: Tree[Int]): Int = {
     tree match {
       case Leaf(a) => a
       case Branch(a, b) => maximum(a) max maximum(b)
     }
   }

  // Ex 3.27
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(a, b) => (1 + depth(a)) max (1 + depth(b))
    }
  }

  // Ex 3.28
  def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(a, b) => Branch(mapTree(a)(f), mapTree(b)(f))
    }
  }

  // Ex 3.29
  def fold[A, B](tree: Tree[A], z: A => B) (f: (B, B) => B): B = {
    tree match {
      case Leaf(a) => z(a)
      case Branch(a, b) => f(fold(a, z)(f), fold(b, z)(f))
    }
  }

  def sizeFold[A](tree: Tree[A]): Int = {
    fold(tree, (a:A) => 1) (_ + _ + 1)
  }

  def maximumFold(tree: Tree[Int]): Int = {
    fold(tree, (a:Int) => a) (_ max _)
  }

  def depthFold[A](tree: Tree[A]): Int = {
    fold(tree, (a: A) => 0) ((a, b) => (a max b) + 1)
  }

  def mapTreeFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold(tree, (a:A) => Leaf(f(a)): Tree[B]) ((a,b) => Branch(a, b))
  }

  def main(args: Array[String]) = {
    val listInt = List(1, 2, 3, 4, 5)
    val listDouble = List(1.0, 2.0, 3.0, 4.0, 5.0)
    val tree = Branch(Leaf(67), Branch(Leaf(34), Leaf(36)))
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

    println("3.16 Add One: " + addOne(listInt))
    println("3.17 Double to String " + doubleToString(listDouble));
    println("3.18 Map " + map(listInt)( a => a + 1))
    println("3.18 Map using foldLeft " + mapUsingFoldLeft(listInt)( a => a + 1))
    println("3.19 Filter " + filter(listInt)(a => a % 2 == 0))
    println("3.20 Flatmap " + flatMap(List(1, 2, 3)) (a => List(a+1, a+1, a+1)))
    println("3.21 Filter Using flatmap " + filterUsingFlatMap(listInt)(a => a % 2 == 0))

    println("3.24 " + hasSubsequence(listInt, List(7, 2)))
    println("3.25 size of the tree " + size(tree))
    println("3.26 maximum of the tree " + maximum(tree))
    println("3.27 depth of the tree " + depth(tree))
    println("3.28 map of the tree " + mapTree(tree)(_+1))

    println("3.29 size of the tree  using fold " + sizeFold(tree))
    println("3.29 maximum of the tree using fold " + maximumFold(tree))
    println("3.29 depth of the tree using fold " + depthFold(tree))
    println("3.29 map of the tree using fold " + mapTreeFold(tree)(_+1))
  }
}
