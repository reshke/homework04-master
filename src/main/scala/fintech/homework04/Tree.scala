package fintech.homework04

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = ???

  // реализовать следующие функции через fold

  def size[A](t: Tree[A]): Int = ???

  def max(t: Tree[Int]): Int = ???

  def depth[A](t: Tree[A]): Int = ???

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = ???
}
