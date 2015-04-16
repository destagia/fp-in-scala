package lesson

object Main2 /* extends App */ {

    // ex 2.1
    /*
       Get the Fibonacci numbers with recursive function.
    */
    def fib(n: Int): Int = n match {
        case x if n == 0 => 0
        case 0 => 1
        case 1 => 1
        case x => fib(n-1) + fib(n-2)
    }

    println(fib(10))

    // ex 2.2
    /*
        Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function.
    */
    def isSorted[A](as: Array[A], orderd: (A, A) => Boolean): Boolean = {
        def check(n: Int): Boolean = n match {
            case n if n < as.length - 1 => orderd(as(n), as(n+1)) && check(n+1)
            case m => true
        }
        check(0)
    }

    println(isSorted(Array(1,2,3,4,5,6,7,8,9,10), ((x:Int, y:Int) => x < y)))
    println(isSorted(Array(1,2,3,4,50,6,7,8,9,10), ((x:Int, y:Int) => x > y)))

    // ex 2.3
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => f(a, _)
    val f = curry((x:Int, y:Float) => "hello")
    println(f(1)(1.0f))

    // ex 2.4
    def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

    // ex 2.5
    def compose[A,B,C](f: B => C, g: A => B): A => C
    = (a: A) => f(g(a))



}