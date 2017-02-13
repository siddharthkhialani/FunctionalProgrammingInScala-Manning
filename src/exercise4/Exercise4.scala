package exercise4

import scala.util.Try

/**
  * Created by siddharthkhialani on 2/6/17.
  */
object Exercise4 {
  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.nonEmpty) Some(xs.sum/xs.length) else None
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }

  def map2[A, B, C](a: Option[A], b: Option[B]) (f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(f(x, _)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    Try {
      a.map(b => f(b) match {
        case Some(c) => c
      })
    }
  }

  def Try2[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {case e: Exception => Left(e)}
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse2(es)(a => a)
  }

  def traverse2[E, A, B](as: List[A]) (f: A => Either[E, B]): Either[E, List[B]] = {
      as.foldLeft(Right(Nil): Either[E, List[B]]) ((b, a) => b.map2(f(a))((p, q) => p ++ List(q)))
  }

  def main(args: Array[String]): Unit = {
    println(None.orElse(Some(23)))
    println(traverse2(List(1,2,3)) (a => Right(a)) )
  }
}
