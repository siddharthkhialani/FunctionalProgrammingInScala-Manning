package exercise5

/**
  * Created by siddharthkhialani on 2/21/17.
  */
object Exercise5 {
  def ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def fibs(a: Long = 0, b: Long = 1): Stream[Long] = Stream.cons(a , fibs(b, a+b) )

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S] (z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).foldLeft(Empty: Stream[A])((b, a) => Cons(() => a._1, () => unfold(a._2)(f)))
  }

  def fibsUsingUnFold(a: Long = 0, b: Long = 1): Stream[Long] = unfold((a, b))(s => Some(s._1, (s._2, s._1 + s._2)))

  def fromUsingUnFold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s+1))

  def constantUsingUnFold[A](a: A): Stream[A] = unfold(a)(s => Some(a, a))

  def onesUsingUnFold: Stream[Int] = unfold(1)(s => Some(s, s))

  def main(args: Array[String]): Unit = {
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ != 1).toList)
    println(ones.forAll(_ != 1))
    println(fibs().take(5).toList)
    println(Cons(() => 1, () => Cons(() => 2, () => Empty)).take(2).toList)
    println(fibsUsingUnFold().take(5).toList)
    println(fromUsingUnFold(10).take(5).toList)
    println(onesUsingUnFold.take(5).toList)
    println(ones.take(5).mapUsingUnFold(_ + 10).toList)
    println(ones.takeUsingUnFold(5).toList)
    println(Stream.cons(2, ones).takeWhileUsingUnFold(_ == 2).toList)
    println(Stream.cons(2, ones).zipAll(Stream.cons(2, Empty)).take(5).toList)
    println(Stream.cons(2, ones).startsWith(Stream.cons(1, Empty)))
    Stream.cons(1, Stream.cons(2, Stream.cons(3, Empty))).tails.toList.foreach(x => println(x.toList))
    println(Stream(1, 2, 3).scanRight(0)(_ + _).toList)
  }
}
