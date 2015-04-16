package util.datastructure


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
    def product2(ns: List[Int]) = foldRight(ns, 1)(_ * _)

    // ex 3.2
    def tail[A](as: List[A]): List[A] = as match {
        case Nil => Nil
        case Cons(h, t) => t
    }

    // ex 3.3
    def setHead[A](as: List[A], element: A): List[A] = as match {
        case Nil => Nil
        case Cons(h, t) => Cons(element, t)
    }

    // ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
        case (Nil, _) => Nil
        case (_, 0) => l
        case (Cons(h, t), x) => drop(t, x-1)
    }

    // ex 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(h, t) =>
            if (f(h)) l
            else dropWhile(t, f)
    }

    // ex 3.6
    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    // ex 3.9
    def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => y + 1)

    // ex 3.10
    // with tail-recursive
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
        def _foldLeft(ass: List[A], res: B): B = ass match {
            case Nil => res
            case Cons(h, t) => _foldLeft(t, f(res, h))
        }
        _foldLeft(as, z)
    }

    // ex 3.11
    def suml(l: List[Int]) = foldLeft(l, 0)(_ + _)

    // ex 3.12
    def productl(l: List[Int]) = foldLeft(l, 1)(_ * _)

    // ex 3.13 (Hard)
    /*
        Can you write foldLeft in terms of foldRight? How about the other way around?
        Implementing foldRight via foldLeft is userful because it lets us implement foldRight
        tail-recursively, which means it works evens for large lists without overflowing the stack.
    */
    // implement foldLeft like foldRight.
    def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
        case Cons(x, xs) => f(foldLeft2(xs, z)(f), x)
        case Nil => z
    }

    // ex 3.14
    def append [A](as1: List[A], as2: List[A]) = foldRight(as1, as2)(Cons(_, _))

    // ex 3.15
    // flatten List of Lists
    // I think my argorhyzhm is not good.
    // memory effecienty is so bad...
    def join[A](as: List[List[A]]): List[A] = as match {
        case Cons(l, m) =>
            def run(xs: List[A]):List[A] = xs match {
                case Nil => Nil
                case Cons(h, Nil) => Cons(h, join(m))
                case Cons(h, t) => Cons(h, run(t))
            }
            run(l)

        case Nil => Nil
    }

    // using the function already implemented.
    def join2[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil:List[A])(foldRight(_, _)(Cons(_, _)))

    // ex 3.16
    def addOne(ints: List[Int]): List[Int] = ints match {
        case Cons(i, t) => Cons(i + 1, addOne(t))
        case Nil => Nil
    }

    // ex 3.17
    def doublesToStrings(doubles: List[Double]): List[String] = doubles match {
        case Cons(d, t) => Cons(d.toString(), doublesToStrings(t))
        case Nil => Nil
    }

    // ex 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] = as match {
        case Cons(a, t) => Cons(f(a), map(t)(f))
        case Nil => Nil
    }

    // ex 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
        case Cons(a, t) =>
            if (f(a)) Cons(a, filter(t)(f))
            else filter(t)(f)
        case Nil => Nil
    }

    // ex 3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = join2(map(as)(f))

    // ex 3.21
    def filterflat[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as){ x =>
            if (f(x)) List(x)
            else Nil
        }

    // ex 3.22
    def addList(xs: List[Int], ys: List[Int]): List[Int] =
        (xs, ys) match {
            case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addList(t1, t2))
            case (Cons(h1, t1), Nil) => Nil
            case (Nil, Cons(h2, t2)) => Nil
            case (Nil, Nil) => Nil
        }

    // ex 3.23
    def zipWith[A, B](xs: List[A], ys: List[A])(f: (A, A) => B): List[B] = {
        (xs, ys) match {
            case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
            case (Cons(h1, t1), Nil) => Nil
            case (Nil, Cons(h2, t2)) => Nil
            case (Nil, Nil) => Nil
        }
    }



}