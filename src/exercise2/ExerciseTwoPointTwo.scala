package exercise2

object ExerciseTwoPointTwo {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean) : Boolean = {
    def loop(n: Int): Boolean = {
      if (as.length <= n) true
      else if (ordered(as(n-1), as(n))) loop(n+1)
      else false
    }
    if (as == null) true
    else loop(1)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b))
  }
}
