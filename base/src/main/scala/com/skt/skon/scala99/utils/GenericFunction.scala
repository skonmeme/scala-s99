package com.skt.skon.scala99.utils

object GenericFunction {

  def listApply[T](list: List[T], k: Int): Option[T] = {
    if (k < 0 || k >= list.length) None
    else Some(list.apply(k))
  }

  def duplicate[T](list: List[T], k: Int): Option[List[T]] = {
    if (k < 0) None
    else Some(list.flatMap(List.fill(k)(_)))
  }

  def logicTable(logic: (Boolean, Boolean) => Boolean): List[Map[(Boolean, Boolean), Boolean]] = {
    val table = List((false, false), (false, true), (true, false), (true, true))
    table.map(x => Map((x._1, x._2) -> logic(x._1, x._2)))
  }

  def gray(k: Int): List[String] = {
    var p = List(List(0), List(1))
    val q = List(List(0), List(1))
    if (k >= 2) {
      for (i <- 2 to k) {
        p = p.flatMap(z => q.map(r => z ++ r))
      }
    }
    p.map(z => z.mkString(""))
  }

  def huffman(list: List[(String, Int)]): List[(String, Int)] = {
    sealed trait Tree[+A]
    case class Branch[A](var left: Tree[A], var right: Tree[A], var value: Int) extends Tree[A]
    case class Leaf[A](value: (String, Int)) extends Tree[A]

    list.length match {
      case 0 => Nil
      case 1 => List((list.head._1, "0"))
      case _ => {
        val l = list.sortBy(_._2).iterator
        var b = Branch(Leaf(list.head), Leaf(list.apply(1)), list.head._2 + list.apply(1)._2)
        l.drop(2).foreach(e => {
          if (b.value <= e._2) {
            b = Branch(b, Leaf(e), b.value + e._2)
          } else {
            b = Branch(Leaf(e), b, b.value + e._2)
          }
        })
        b.
      }

  }
}
