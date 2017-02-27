package exercise5

import exercise5.Exercise5.unfold

/**
  * Created by siddharthkhialani on 2/21/17.
  */
sealed trait Stream [+A] {

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def toList: List[A] = {
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) if n <= 0 => Empty
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) if n > 0 => take(n-1)
      case z if n <= 0 => z
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if(p(a)) Cons(() => a, () => b) else Empty)
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B]) ((a, b) => Cons(() => f(a), () => b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (f(a)) Cons(() => a, () => b) else b)
  }

  def append[AA >: A](c: Stream[AA]): Stream[AA] = {
    foldRight(c)((a, b) => Cons(() => a, () => b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B]) ((a, b) => f(a).append(b))
  }

  def mapUsingUnFold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) => Some(f(h()), t())
    }
  }

  def takeUsingUnFold(n: Int): Stream[A] = {
    unfold((this, n)) (s => s._1 match {
      case Empty => None
      case Cons(h, t) if s._2 > 0 => Some(h(), (t(), s._2 - 1))
      case z => None
    })
  }

  def takeWhileUsingUnFold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Empty => None
      case Cons(h, t) if p(h()) => Some(h(), t())
      case z => None
    }
  }
  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(a, b), Cons(c, d)) => Some(f(a(), c()), (b(), d()))
      case z => throw new Exception("Both streams should have same number of elements")
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(a, b), Cons(c, d)) => Some((Some(a()), Some(c())), (b(), d()))
      case (Cons(a, b), Empty) => Some((Some(a()), None), (b(), Empty))
      case (Empty, Cons(c, d)) => Some((None, Some(c())), (Empty, d()))
    }
  }

  def startsWith[A](s: Stream[A]): Boolean = {
    this.zipAll(s).foldRight(true)((a, b) => if(a._2.isDefined) a._1.contains(a._2.get) && b else true)
  }

  def tails: Stream[Stream[A]] = {
    this match {
      case Cons(a, b) => Stream.cons(Cons(a, b), b().tails)
      case Empty => Stream.cons(Empty, Empty)
    }
  }

  def hasSubsequence[A](s: Stream[A]) = {
    tails exists(_ startsWith s)
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    this match {
      case Cons(a, b) => val s =  b().scanRight(z)(f); Stream.cons(f(a(), s.headOption.get), s)
      case Empty => Stream.cons(z, Empty)
    }
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
  }
}
