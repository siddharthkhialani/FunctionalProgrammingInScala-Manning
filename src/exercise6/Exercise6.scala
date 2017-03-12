package exercise6

/**
  * Created by siddharthkhialani on 3/5/17.
  */
object Exercise6 {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val next = rng.nextInt
    if(next._1 >= 0) next else (next._1 + (- Int.MinValue), next._2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val next = nonNegativeInt(rng)
    if(next._1 == Int.MaxValue) (0.0, next._2) else (next._1.toDouble/Int.MaxValue, next._2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val nextInt = nonNegativeInt(rng)
    val nextDouble = double(nextInt._2)
    ((nextInt._1, nextDouble._1), nextDouble._2)
  }

  def doubleInt(rng: RNG) :((Double, Int), RNG) = {
    val nextDouble = double(rng)
    val nextInt = nonNegativeInt(nextDouble._2)
    ((nextDouble._1, nextInt._1), nextInt._2)
  }

  def double3(rng: RNG):((Double, Double, Double), RNG) = {
    val nextDouble1 = double(rng)
    val nextDouble2 = double(nextDouble1._2)
    val nextDouble3 = double(nextDouble2._2)
    ((nextDouble1._1, nextDouble2._1, nextDouble3._1), nextDouble3._2)
  }

  def ints(count: Int) (rng: RNG): (List[Int], RNG) = {
    (0 until count).foldLeft((Nil: List[Int], rng)) ((b, a) => {val next = b._2.nextInt; (b._1 ++ List(next._1), next._2)})
  }

  def map[A, B](s: Rand[A]) (f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def doubleUsingMap(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt) (i => if(i != Int.MaxValue) i.toDouble/Int.MaxValue else 0.0) (rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = {
    rng => {
      val(a, rng2) = ra(rng)
      val(b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldLeft((Nil: List[A], rng)) ((b, a) => {val next = a(b._2); (b._1 ++ List(next._1), next._2)})
    }
  }

  def intsUsingSequence(count: Int) (rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)((rng: RNG) => rng.nextInt)) (rng)
  }

  def flatMap[A, B](f: Rand[A]) (g: A => Rand[B]): Rand[B] = { rng =>
    val aTuple = f(rng)
    g(aTuple._1) (aTuple._2)
  }

  def mapIntermsOfFlatMap[A, B](s: Rand[A]) (f: A => B): Rand[B] = {
    flatMap(s)(a => rng => (f(a), rng))
  }

  def map2IntermsOfFlatMap[A, B, C](ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))
  }

  type Rand[+A] = RNG => (A, RNG)

  //type State[S, +A] = S => (A, S)

  //case class State[S, +A](run: S => (A, S))

  //type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      x <- State.sequence(inputs.map(input => State.modify((a: Machine) => a.nextState(input))) )
      y <- State.get
    } yield (y.candies, y.coins)
  }

  def main(args: Array[String]): Unit = {
    println(Int.MaxValue)
    println(nonNegativeInt(SimpleRNG(42)))
    println(double(SimpleRNG(42)))
    println("Ints" + ints(3)(SimpleRNG(42)))
    println(doubleUsingMap(SimpleRNG(42)))
    println("IntsUsingSequence" + intsUsingSequence(3)(SimpleRNG(42)))
    println("Map: " + map(rng => rng.nextInt)(a => a.toString + "hello")(SimpleRNG(42)))
    println("Map in terms of flatmap: " + mapIntermsOfFlatMap(rng => rng.nextInt)(a => a.toString + "hello")(SimpleRNG(42)))

    println("Map2: " + map2(rng1 => rng1.nextInt, rng2 => rng2.nextInt)((a, b) => a * b)(SimpleRNG(42)))
    println("Map2: " + map2IntermsOfFlatMap(rng1 => rng1.nextInt, rng2 => rng2.nextInt)((a, b) => a * b)(SimpleRNG(42)))

    println("Machine: " + simulateMachine(List(Coin, Turn, Coin, Turn): List[Input]).run(Machine(true, 10, 4)))
  }
}
