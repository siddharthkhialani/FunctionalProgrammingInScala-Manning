package exercise6

case class State[S, +A](run: S => (A, S)) {

  def map[B] (f: A => B): State[S, B] = {
    State((rng: S) => {
      val (a, rng2) = run(rng)
      (f(a), rng2)
    })
  }

  def map2[B, C](rb: State[S, B]) (f: (A, B) => C): State[S, C] = {
    State(rng => {
      val(a, rng2) = this.run(rng)
      val(b, rng3) = rb.run(rng2)
      (f(a, b), rng3)
    })
  }
  def flatMap[B] (g: A => State[S, B]): State[S, B] = State(rng => {
    val aTuple = this.run(rng)
    g(aTuple._1).run(aTuple._2)
  })
}

object State {
  def unit[S, A] (a: A): State[S, A] = {
    State((s:S) => (a, s))
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    State(rng => {
      fs.foldLeft((Nil: List[A], rng)) ((b, a) => {val next = a.run(b._2); (b._1 ++ List(next._1), next._2)})
    })
  }
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}