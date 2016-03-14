package fpinscala.laziness

import Stream._
trait Stream[+A] {
  def toList: List[A] = foldRight(Nil:List[A])(_::_)

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
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => cons(h(), t().take(n-1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty:Stream[A])((a,b)=>{if (p(a)) cons(a,b) else b})

  def dropWhile2(p: A => Boolean): Stream[A] = foldRight(empty:Stream[A])((a,b)=>{if (p(a)) b else cons(a,b)})

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def headOption: Option[A] = this match {
    case Cons(h,_) => Some(h())
    case _ => None
  }

  def headOption2: Option[A] = foldRight(None:Option[A])((a,b)=>Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b)=>cons(f(a),b))
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a,b)=> if (f(a)) cons(a,b) else b)
  def append[B>:A](bs: Stream[B]): Stream[B] = foldRight(bs)((a,b)=>cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b)=>f(a).append(b))

  // Exercise 5.13
  def map2[B](f: A => B): Stream[B] = unfold(this){
    case Empty => None
    case Cons(h,t) => Some((f(h())),t())}
  def take2(n: Int): Stream[A] = unfold[A,(Stream[A],Int)]((this,n)){
    case (Cons(h,t),n) if n > 0 => Some((h(), (t(),n-1)))
    case _ => None
  }
  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h,t) if (p(h())) => Some((h(),t()))
    case _ => None
 }
  def zipWith[B,C](bs: Stream[B])(f: (A,B) => C): Stream[C] = unfold[C,(Stream[A],Stream[B])]((this,bs)){
    case (Cons(ha,ta),Cons(hb,tb)) => Some((f(ha(),hb()),(ta(),tb())))
    case _ => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold[(Option[A],Option[B]),(Stream[A],Stream[B])]((this,s2)){
    case (Cons(ha,ta), Cons(hb,tb)) => Some((Some(ha()),Some(hb())),(ta(),tb()))
    case (Cons(ha,ta), Empty) => Some((Some(ha()),None),(ta(),Empty))
    case (Empty, Cons(hb,tb)) => Some((None,Some(hb())),(Empty,tb()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = zipAll[B](s).map[Boolean]{ case (a,b) => a==b||b==None}.forAll(_==true)

  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(h,t) => Some((cons(h(),t()), t()))
    case Empty => None
  }.append(Stream(Stream()))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z),z){ case (a,(sacc,acc)) => {
      lazy val fa = f(a,acc)
      (cons(fa,sacc),fa)}}._1

  def hasSubsequence[A](s: Stream[A]): Boolean = tails.exists(_ startsWith s)

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
  def constant(n: Int): Stream[Int] = Stream.cons(n, constant(n))
  def from(n: Int): Stream[Int] = Stream.cons(n, Stream.from(n+1))

  val fibs: Stream[Int] = {
    def go(a:Int ,b:Int): Stream[Int] =
      cons(a,go(b,a+b))
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def go(s1: S): Stream[A] = f(s1) match {
      case None => empty
      case Some((a,s2)) => cons(a,go(s2))
    }
    go(z)
  }

  val fibs2: Stream[Int] = unfold[Int,(Int,Int)](0,1){case (s1,s2)=>Some((s1,(s2,s1+s2)))}

}

object TestStream {

  import Stream._

  def main(args: Array[String]): Unit = {
    val s1 : Stream[Int] = Stream(1,2,3,4,5)
    val se : Stream[Int] = empty[Int]
    val si1 : Stream[Int] = constant(1)
    val si2 : Stream[Int] = constant(2)
    println(s1)
    println(s1.toList)
    println(s1.drop(2).toList)
    println(s1.drop(6).toList)
    println(s1.take(3).toList)
    println(s1.take(6).toList)
    println(s1.take2(3).toList)
    println(s1.take2(6).toList)

    println("\n")
    println(s1.takeWhile(_<4).toList)
    println(s1.takeWhile2(_<4).toList)
    println(s1.takeWhile3(_<4).toList)
    println(s1.forAll(_<4))
    println(s1.forAll(_<7))
    println(s1.headOption)
    println(s1.headOption2)
    println(se.headOption)
    println(se.headOption2)

    println("\n")
    println(s1.map(100+_).toList)
    println(s1.map2(100+_).toList)
    println(s1.filter(_%2==0).toList)
    println(s1.append(s1).toList)
    println(s1.append(si1).append(si2).take(7).toList)
    println(s1.flatMap(x=>Stream(x,x)).toList)
    println(s1.flatMap(n=>constant(n)).take(5).toList)

    println("\n")
    println(fibs.take(20).toList)
    println(fibs2.take(20).toList)

    println("\n")
    println(s1.startsWith(se))
    println(s1.startsWith(s1))
    println(Stream(1,2,3).startsWith(s1))
    println(s1.startsWith(Stream(1,2,3)))
    println(s1.startsWith(Stream(2,3)))
    println(se.startsWith(s1))

    println("\n")
    println(s1.tails.toList.map(l => l.toList))

    println("\n")
    println(s1.hasSubsequence(s1))
    println(s1.hasSubsequence(se))
    println(s1.hasSubsequence(Stream(2,3)))
    println(s1.hasSubsequence(Stream(2,4)))
    println(s1.hasSubsequence(si1))

    println("\n")
    println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  }
}