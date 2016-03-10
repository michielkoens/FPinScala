package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(b) => b
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }
  def flatMap2[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => this
    case None => ob
  }
  def orElse2[B>:A](ob: => Option[B]): Option[B] = this.map(a => Some(a)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }
  def filter2(f: A => Boolean): Option[A] = this.flatMap(a => { if (f(a)) Some(a) else None })
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m,2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aa => b.map(bb => f(aa,bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil):Option[List[A]])((a,b)=>map2[A,List[A],List[A]](a,b)(_::_))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil):Option[List[B]])((a,b)=>map2[B,List[B],List[B]](f(a),b)(_::_))

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x=>x)
}

object TestOption {
  import Option._

  def main(args: Array[String]): Unit = {
    val v1: Option[Int] = Some(1)
    val v2: Option[Int] = Some(2)
    val vn: Option[Int] = None

    println("val v1: Option[Int] = Some(1)")
    println("val v2: Option[Int] = Some(2)")
    println("val vn: Option[Int] = None")

    println("\nv1.map(2+_) =")
    println(v1.map(2+_))
    println("vn.map(2+_) =")
    println(vn.map(2+_))
    println("v1.getOrElse(2) =")
    println(v1.getOrElse(2))
    println("vn.getOrElse(2) =")
    println(vn.getOrElse(2))


    println("\nv1.flatMap(x=>Some(x*11)) =")
    println(v1.flatMap(x=>Some(x*11)))
    println("vn.flatMap(x=>Some(x*11)) =")
    println(vn.flatMap(x=>Some(x*11)))
    println("v1.flatMap2(x=>Some(x*11)) =")
    println(v1.flatMap(x=>Some(x*11)))
    println("vn.flatMap2(x=>Some(x*11)) =")
    println(vn.flatMap(x=>Some(x*11)))

    println("\nv1.orElse(v2) =")
    println(v1.orElse(v2))
    println("vn.orElse(v2) =")
    println(vn.orElse(v2))
    println("v1.orElse2(v2) =")
    println(v1.orElse2(v2))
    println("vn.orElse2(v2) =")
    println(vn.orElse2(v2))

    println("\nv1.filter(1==_) =")
    println(v1.filter(1==_))
    println("v2.filter(1==_) =")
    println(v2.filter(1==_))
    println("vn.filter(1==_) =")
    println(vn.filter(1==_))
    println("v1.filter2(1==_) =")
    println(v1.filter2(1==_))
    println("v2.filter2(1==_) =")
    println(v2.filter2(1==_))
    println("vn.filter2(1==_) =")
    println(vn.filter2(1==_))

    println("\nsequence(List(Some(1),Some(2),Some(3))) =")
    println(sequence(List(Some(1),Some(2),Some(3))))
    println("sequence(List(Some(1),None,Some(3))) =")
    println(sequence(List(Some(1),None,Some(3))))
    println("sequence2(List(Some(1),Some(2),Some(3))) =")
    println(sequence2(List(Some(1),Some(2),Some(3))))
    println("sequence2(List(Some(1),None,Some(3))) =")
    println(sequence2(List(Some(1),None,Some(3))))

    println("\nvariance(List()) =")
    println(variance(List()))
    println("variance(List(1.0,1.0,1.0)) =")
    println(variance(List(1.0,1.0,1.0)))
    println("variance(List(1,2,3,4,5,6,7,8,9,10)) =")
    println(variance(List(1,2,3,4,5,6,7,8,9,10)))

  }
}