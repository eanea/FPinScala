package Part1.Chapter3

import Part1.Chapter3.Tree.Maximum.Max

sealed trait Tree[+A]

object Tree {
  case class Leaf[A](value: A)                        extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Leaf {
    implicit class RichLeaf(l: Leaf[Int]) {
      def max(l2: Leaf[Int])(implicit conv: Max[Leaf[Int]]) = conv.max(l, l2)
    }
  }

  object Maximum {
    trait Max[A] {
      def max(v1: A, v2: A): A
    }

    implicit object LeafMax extends Max[Leaf[Int]] {
      override def max(v1: Leaf[Int], v2: Leaf[Int]): Leaf[Int] =
        if (v1.value >= v2.value) v1
        else v2
    }
  }

  /**
    * EXERCISE 3.25
    */
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  /**
    * EXERCISE 3.26
    */
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v)      => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
    * EXERCISE 3.27
    */
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_)      => 1
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  /**
    * EXERCISE 3.28
    */
  def mapTree[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(mapTree(l)(f), mapTree(r)(f))
  }

  /**
    * EXERCISE 3.29
    */
  def foldTree[A, B](tree: Tree[A], f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => g(foldTree(l, f)(g), foldTree(r, f)(g))
    }

  def mapTreeWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    foldTree[A, Tree[B]](tree, x => Leaf(f(x)))(Branch(_, _))

  def maximumWithFold(tree: Tree[Int]): Leaf[Int] =
    foldTree(tree, Leaf[Int])((x, acc) => x max acc)

  def depthWithFold[A](tree: Tree[A]): Int =
    foldTree[A, Int](tree, _ => 1)(1 + _ max 1 + _)

  def sizeWithFold[A](tree: Tree[A]): Int =
    foldTree[A, Int](tree, _ => 1)(1 + _ + _)
}
