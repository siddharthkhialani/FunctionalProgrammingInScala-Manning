package excercise2

class ExerciseTwoPointFive {
  def compose[A, B, C] (f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
