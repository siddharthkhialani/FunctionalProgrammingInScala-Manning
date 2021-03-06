package exercise2

class ExerciseTwoPointThree {
  def curry[A, B, C] (f: (A, B) => C) : A => (B => C) = {
    a => b => f(a, b)
  }
}
