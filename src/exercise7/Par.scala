package exercise7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import exercise5.Empty

/**
  * Created by siddharthkhialani on 3/19/17.
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =    (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    es => unit(
      ps.map(p => p(es).get())
    )(es)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs: List[Par[Option[A]]] = as.map(asyncF(a => if(f(a)) Some(a) else None))
    map(sequence(fbs))(a => a.flatten)
  }
}

private case class UnitFuture[A](get: A) extends Future[A] {
  def isDone = true
  def get(timeout: Long, units: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}


