package util.datastructure

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    // ex 3.25
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(v) => 1
        case Branch(l, r) => size(l) + size(r) + 1
    }

    // ex 3.26
    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    // ex 3.27
    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(v) => 1
        case Branch(l, r) => (depth(l) + 1) max (depth(r) + 1)
    }

    // ex 3.28
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    // ex 3.29
    /* Implement fold function. */
}