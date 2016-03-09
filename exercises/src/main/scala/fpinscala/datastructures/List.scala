package fpinscala.datastructures

import sun.font.TrueTypeFont

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, xs) if n > 0 => drop(xs, n-1)
    case _ => l
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(x, Cons(_, Nil)) => Cons(x, Nil)
    case Cons(x, xs) => Cons(x, init(xs))
    case _ => Nil
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, n) => n+1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft[A,List[A]](l, Nil)((a,b)=>Cons(b,a))

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b:B)=>b)((a,ff)=>(bb=>f(ff(bb),a)))(z)

  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b:B)=>b)((ff,a)=>(bb=>f(a,ff(bb))))(z)

  def append2[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((a,b)=>Cons(a,b))

  def concatenate[A](xss: List[List[A]]): List[A] = foldRight(xss, Nil:List[A])(append2[A])

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l,Nil:List[B])((a,bs)=>Cons(f(a),bs))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l,Nil:List[A])((a,as)=>{if (f(a)) Cons(a,as) else as})

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil:List[B])((a,bs)=>append(f(a),bs))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a =>{ if (f(a)) List(a) else Nil})

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = (as,bs) match {
    case (Cons(a,as1), Cons(b,bs1)) => Cons(f(a,b), zipWith(as1,bs1)(f))
    case  _ => Nil
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(x,xs),Cons(y,ys)) => x == y && startsWith(xs,ys)
    case _ => false
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(x,xs),Cons(y,ys)) => (x==y && startsWith(xs,ys)) || hasSubsequence(xs, sub)
    case _ => false
  }
}

object testList {
  import List._

  def main(args: Array[String]): Unit = {
    val xs: List[Int] = List(1,2,3,4,5)

    println("xs = List(1,2,3,4,5)")
    println("\ntail(xs) = ")
    println(tail(xs))
    println("setHead(xs, 99) = ")
    println(setHead(xs, 99))
    println("drop(xs, 3) = ")
    println(drop(xs, 3))
    println("dropWhile(xs, _ < 3) = ")
    println(dropWhile[Int](xs, _ < 3))
    println("init(xs) = ")
    println(init(xs))
    println("length(xs) = ")
    println(length(xs))
    println("\nfoldLeft(xs, 0)( _ - _) = ")
    println(foldLeft(xs, 0)( _ - _))
    println("foldRight(xs, 0)( _ - _) = ")
    println(foldRight(xs, 0)( _ - _))
    println("foldLeft2(xs, 0)( _ - _) = ")
    println(foldLeft2(xs, 0)( _ - _))
    println("foldRight2(xs, 0)( _ - _) = ")
    println(foldRight2(xs, 0)( _ - _))
    println("\nreverse(xs) = ")
    println(reverse(xs))
    println("\nappend2(xs, List(6,7,8) = ")
    println(append2(xs, List(6,7,8)))
    println("concatenate([xs, List(6,7,8), List(9,10,11)] = ")
    println(concatenate(List(xs, List(6,7,8), List(9,10,11))))
    println("\nmap(xs)(2+_) = ")
    println(map(xs)(2+_))
    println("map(xs)(x=>x%2==0) = ")
    println(map(xs)(x=>x%2==0))
    println("filter(xs)(x=>x%2==0) = ")
    println(filter(xs)(x=>x%2==0))
    println("\nflatMap(xs)(x=>List(x,x)) = ")
    println(flatMap(xs)(x=>List(x,x)))
    println("filter2(xs)(x=>x%2==0) = ")
    println(filter2(xs)(x=>x%2==0))
    println("\nzipWith(xs, reverse(xs))(_ - _) =")
    println(zipWith(xs, reverse(xs))(_ - _))
    println("\nhasSubsequence(xs, List()) =")
    println(hasSubsequence(xs, List()))
    println("hasSubsequence(xs, List(1,2)) =")
    println(hasSubsequence(xs, List(1,2)))
    println("hasSubsequence(xs, List(1,2,3,4,5)) =")
    println(hasSubsequence(xs, List(1,2,3,4,5)))
    println("hasSubsequence(xs, List(3,4,5)) =")
    println(hasSubsequence(xs, List(3,4,5)))
    println("hasSubsequence(xs, List(6)) =")
    println(hasSubsequence(xs, List(6)))
    println("hasSubsequence(xs, List(1,3,5)) =")
    println(hasSubsequence(xs, List(1,3,5)))
  }

}