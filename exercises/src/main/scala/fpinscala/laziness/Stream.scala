package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = 
    this match {
      case Empty => Empty
      case Cons(h, t) if (n <= 0) => Empty
      case Cons(h, t) => cons(h(), t().take(n-1))
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) if (n <= 0) => cons(h(), t())
      case Cons(h, t) => t().drop(n-1)
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if (p(a)) cons(a,b) else Empty)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((h, t) => if (f(h)) cons(h, t) else t)

  def append[B >: A](as: => Stream[B]): Stream[B] =
    foldRight(as)(cons(_, _))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  def tails: Stream[Stream[A]] =
    unfold(this){
      case Cons(_, t) => Some(this, t())
      case Empty => None
    }

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def map2[B](f: A => B): Stream[B] = 
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def take2(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B,C](bs: Stream[B])(f:(A,B) => C):Stream[C] =
    unfold((this, bs)) {
      case (Cons(x, xs), Cons(y, ys)) => Some((f(x(), y()), (xs(), ys())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(x, xs), Cons(y, ys)) => Some((Some(x()), Some(y())), (xs(), ys()))
      case (Cons(x, xs), Empty) => Some((Some(x()), None), (xs(), Empty))
      case (Empty, Cons(y, ys)) => Some((None, Some(y())), (Empty, ys()))
      case _ => None
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  def from(n: Int): Stream[Int] = cons(n, from(n+1))
  val fibs: Stream[Int] = {
    def go(a: Int, b: Int):Stream[Int] =
      cons(a, go(b, a+b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some(v) => cons(v._1, unfold(v._2)(f))
    }

  val fibs2: Stream[Int] = unfold((0, 1))(v => Some((v._1, (v._2, v._1 + v._2))))
  def from2(n: Int): Stream[Int] = unfold(n)(v => Some((v, v+1)))
  def constant2[A](a: A): Stream[A] = unfold(a)(v => Some((v, v)))
  val ones2: Stream[Int] = unfold(1)(_ => Some(1,1))

  val test_stream = Stream.cons({println("1"); 1}, Stream.cons({println("2"); 2}, Stream.cons({println("3"); 3}, Stream.empty)))
}
