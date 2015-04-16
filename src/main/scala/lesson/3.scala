package lesson

import util.{datastructure => D}
import util.datastructure.List._

object Main3 extends App {

    // ex 3.1
    val x = D.List(1,2,3,4,5) match {
        case D.Cons(x, D.Cons(2, D.Cons(4, _))) => x
        case D.Nil => 42
        case D.Cons(x, D.Cons(y, D.Cons(3, D.Cons(4, _)))) => x
        case D.Cons(h, t) => h + sum(t)
        case _ => 101
    }

    val list = D.List(1,2,3,4,5,6,7,8)
    println("first length : " + list)
    println("seconds length : " + tail(list))
    println("replace head to 10 : " + setHead(list, 10))
    println("drop 5 : " + drop(list, 5))
    println("drop while the element is 6 : " + dropWhile(list, (x:Int) => x == 6))
    println("init : " + init(list))
    println("sum2 : " + sum2(list))
    println("product2 : " + product2(list))
    println("length : " + length(list))
    println("foldLeft : " + foldLeft(list, 0)((x, y) => x - y))
    println("foldLeft2 : " + foldLeft2(list, 0)((x, y) => x - y))
    println("foldRight : " + foldRight(list, 0)((x, y) => x - y))
    println("append in terms of foldRight : " + append(D.List(1,2,3,4), D.List(10,20,30,40)))
    println("join the list of lists : " + join(D.List(D.List(1,2,3,4), D.List(1,2,3,4), D.List(10,10))))
    println("join2 the list of lists : " + join2(D.List(D.List(1,2,3,4), D.List(1,2,3,4), D.List(10,10))))
    println("increment all element of the integer List : " + addOne(D.List(0,0,0,0)))
    println("stringify all element of the double List : " + doublesToStrings(D.List(1.0, 2.0, 3.0, 4.5)) )
    println("map : " + map(D.List(1,2,3,4))(_ + 10.5))
    println("filter : " + filter(D.List(1,2,3,4))(_ % 2 == 0))
    println("flatMap : " + flatMap(D.List(1,2,3,4))(i => D.List(i, i)))
    println("filter via flatMap : " + filterflat(D.List(1,2,3,4,5,6))(_ % 2 == 0) )
    println("addList : " + addList(D.List(1,2,3), D.List(4,5,6)))
    println("addList : " + zipWith(D.List(1,2,3), D.List(4,5,6))((x, y) => (x, y)))

    // ex 3.7
    /*
        Can product, implemented using foldRight, immediately halt the recursion and return 0.0
        if it encounters a 0.0?
    */
    /*
        The function foldRight works with the structure of List, so I can't controll by the indeed value of head.
        Therefore I can't halt the recursion and return 0.0 in the case.
    */


    // ex 3.8
    /*
        See what happens when you pass Nil and Cons themsleves to foldRight, like the flowing code.
    */
    println(foldRight(D.List(1,2,3), D.Nil:D.List[Int])(D.Cons(_, _)))
    /*
        the foldRight is the id function of the data structure, List?
    */

}