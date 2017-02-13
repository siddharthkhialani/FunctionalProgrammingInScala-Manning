package exercise4

/**
  * Created by siddharthkhialani on 2/12/17.
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(b) => Left(b)
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => f(a)
      case Left(b) => Left(b)
    }
  }
  def orElse[EE >: E, B >:A] (b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(x) => b
      case Left(e) => Left(e)
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f:(A, B) => C): Either[EE, C] ={
    this.flatMap(a => b.map(f(a,_)))
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {case e: Exception => Left(e)}
  }
}
case class Left[+E] (value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

