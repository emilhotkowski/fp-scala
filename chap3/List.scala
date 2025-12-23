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
    
    def setHead[A](newHead: A, l: List[A]): List[A] = l match
        case Nil => sys.error("Cannot set new head on an empty list")
        case Cons(head, tail) => Cons(newHead, tail)

    def drop[A](as: List[A], n: Int): List[A] = 
        if (n == 0) {
            as
        } else {
            as match
                case Nil => sys.error("List is too short")
                case Cons(head, tail) => drop(tail, n-1)
        }

    def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
        as match
            case Nil => Nil
            case Cons(head, tail) => if(f(head)) {
                dropWhile(tail, f)
            } else {
                Cons(head, tail)
            }
        
    def init[A](as: List[A]): List[A] =
        as match
            case Nil => sys.error("Empty list")
            case Cons(head, Nil) => Nil
            case Cons(head, tail) => Cons(head, init(tail))
    
    def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = 
        as match
            case Nil => acc
            case Cons(head, tail) => foldRight(tail, f(head, acc), f)     

    def length[A](as: List[A]): Int =
        foldRight(as, 0, (_, acc) => acc+1)  
}