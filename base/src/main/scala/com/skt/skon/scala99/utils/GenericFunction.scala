package com.skt.skon.scala99.utils

import com.skt.skon.scala99.datatypes.{HuffmanBranch, HuffmanLeaf}

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

  def huffman(list: List[(String, Int)]): List[(String, String)] = {
    list.length match {
      case 0 => Nil
      case 1 => List((list.head._1, "0"))
      case _ => {
        val l = list.sortBy(_._2).map(x => HuffmanLeaf(x._1, x._2)).iterator
        val h = l.next
        val t = l.next
        var b = HuffmanBranch(h, t, h.count + t.count)

        var buffer: Option[HuffmanLeaf[String]] = None
        while (l.hasNext) {
          if (buffer.isEmpty) buffer = Some(l.next)
          else {
            b = b.add(buffer.get, l.next)
            buffer = None
          }
        }
        if (buffer.isDefined) b = b.add(buffer.get)
        b.code
      }
    }
  }
}
