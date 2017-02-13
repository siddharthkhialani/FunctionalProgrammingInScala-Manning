package exercise4

/**
  * Created by siddharthkhialani on 2/5/17.
  */

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(a) => a
      case None => default
    }
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this.map(a => Some(a)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    if( map(f).getOrElse(false) ) map(a => a) else None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]