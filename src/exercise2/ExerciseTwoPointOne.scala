package exercise2

object ExerciseTwoPointOne {
  def fib(n: Int) = {
    def loop(b: Int, acc: Int, n: Int): Int = {
      if(n <= 0) acc else loop(acc, b + acc, n - 1)
    }

    if(n == 1) {
      0
    } else if(n == 2) {
      1
    } else loop(1, 1, n - 2)
  }

  def main(args: Array[String]) : Unit = {
    println(fib(5))
  }
}