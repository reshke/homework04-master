package fintech.homework04

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold

  private case class Node[B](current_depth : Int)(val value : B)

  @tailrec
  def fold_times[A,B](current_depth : Int)(acc : Seq[Node[B]])(seq: Seq[Tree[A]])(f: A => B)(g: (B,B) => B): B = {
    var newAcc: Seq[Node[B]] = acc
    var new_depth = current_depth

    while (newAcc.length > 1 && newAcc.head.current_depth == newAcc.tail.head.current_depth) {
      val head = newAcc.head
      newAcc = newAcc.tail

      val el = Node(head.current_depth - 1)(g(head.value, newAcc.head.value))
      newAcc = el +: newAcc.tail
      new_depth -= 1
    }


    if (seq.nonEmpty)
      seq.head match {
        case Leaf(value) => fold_times(new_depth)(Node(new_depth)(f(value)) +: newAcc)(seq.tail)(f)(g)
        case Branch(left, right) => fold_times(new_depth + 1)(newAcc)(right +: left +: seq.tail)(f)(g)
      }
    else
      newAcc.head.value
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    val seq: Seq[Tree[A]] = Seq(t)
    val acc: Seq[Node[B]] = Seq.empty[Node[B]]
    fold_times(0)(acc)(seq)(f)(g)
    //    t match {
    //      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    //      case Leaf(value) => f(value) // это без tailrec, зато работает и всего 2 строки
    //    }
  }

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = {
    fold[A, Int](t)((_: A) => 1)((x: Int, y  :Int) => x + y + 1)
  }

  def max(t: Tree[Int]): Int = {
    fold[Int, Int](t)((x: Int) => x)((x: Int, y : Int) => math max(x, y))
  }

  def depth[A](t: Tree[A]): Int = {
    fold[A, Int](t)((_ : A) => 0)((x : Int, y: Int) => math.max(x, y) + 1)
  }

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    val g = (x : Tree[B], y: Tree[B]) => new Branch[B](x, y)
    fold[A, Tree[B]](t)((x : A) => Leaf(f(x)))(g)
  }
}



