package exercise8

import exercise6.{RNG, SimpleRNG, State}

/**
  * Created by siddharthkhialani on 5/7/17.
  */
case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B] (f: A => Gen[B]) : Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(rng => ((rng.nextInt._1 % (stopExclusive - start)) + start, rng.nextInt._2)))
  }

  def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))

  def boolean: Gen[Boolean] = Gen(choose(0, 2).sample.map(a => if(a == 1) true else false))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](a: Gen[A], b: Gen[A]): Gen[A] = boolean.flatMap(i => if(i) a else b)
  //Gen(State(rng => {val r = choose(0,2).sample.run(rng); if(r == 0) a.sample.run(r._2) else b.sample.run(r._2)} ))
}

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
}
