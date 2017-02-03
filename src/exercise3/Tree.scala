package exercise3

/**
  * Created by siddharthkhialani on 1/28/17.
  */
sealed trait Tree[+A]
case class Leaf[A]( value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
