package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def fold[A,B](t: Tree[A])(fl: A=>B, fb: (B,B)=>B): B = t match {
    case Leaf(a) => fl(a)
    case Branch(l, r) => fb(fold(l)(fl,fb),fold(r)(fl,fb))
  }
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = fold[A,Tree[B]](t)(a=>Leaf(f(a)),(l,r)=>Branch(l,r))

  def size[A](t: Tree[A]): Int = fold[A,Int](t)(_=>1,(l,r)=>l+r+1)

  def maximum(t: Tree[Int]): Int = fold[Int,Int](t)(x=>x,(l,r)=> l max r)

  def depth[A](t: Tree[A]): Int = fold[A,Int](t)(_=>1,(l,r)=> 1 + (l max r))
}

object TestTree {
  import Tree._

  def main(args: Array[String]): Unit = {
    val t: Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Branch(Leaf(4), Leaf(5)))

    println("t = Branch(Branch(1, Branch(2, 3)), Branch(4, 5)")
    println("\nsize(t) =")
    println(size(t))
    println("\nmaximum(t) =")
    println(maximum(t))
    println("\ndepth(t) =")
    println(depth(t))
    println("\nmap(t,2+_) =")
    println(map(t)(2+_))
  }
}