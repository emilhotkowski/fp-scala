package chap3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](elem: A*): List[A] = {
        if(elem.isEmpty) {
            Nil
        } else {
            Cons(elem.head, apply(elem.tail*))
        }
    }

    def sum(list: List[Int]): Int = list match
        case Nil => 0
        case Cons(head, tail) => head + sum(tail)
    
    def tail[A](l: List[A]): List[A] = l match
        case Nil => sys.error("Empty list error")
        case Cons(head, tail) => tail
    
}