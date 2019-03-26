package com.skt.skon.scala99

import com.skt.skon.scala99.datasets.Lists
import com.skt.skon.scala99.implicits.ListExtention
import com.skt.skon.scala99.utils.{WithOption, WithTag}

object Questions {

  implicit def listToListExtention[T](list: List[T]): ListExtention[T] = new ListExtention(list)

  def main(args: Array[String]): Unit = {
    // P1: Find the last element of a list.
    val a1 = Lists.p1.last
    // List.q1.head
    // List.q1.tail
    println("P1: " + a1)

    // P2: Find the last but one element of a list.
    val a2 = Lists.p1.reverse.apply(1)
    // Lists.q1.reverse.iterator
    println("P2: " + a2)

    // P3: Find the Kth element of a list.
    val k = 3
    //val a3 = WithOption.listApply(Lists.p1, k)
    val a3 = Lists.p1.applyWithOption(k)
    //a3 match {
    //  case None => println("Out of range")
    //  case _ => println(a3)
    //}
    println("P3: " + a3)

    // P4: Find the number of elements of a list.
    val a4 = Lists.p1.length
    println("P4: " + a4)

    // P5: Reverse a list.
    val a5 = Lists.p1.reverse
    println("P5: " + a5)

    // P6: Find out whether a list is a palindrome.
    val a6 = (Lists.p6 == Lists.p6.reverse)
    println("P6: " + a6)

    // P7: Flatten a nested list structure.
    val a7 = WithTag.flatMap[Any](Lists.p7)
    println("P7: " + a7)

    // P8: Eliminate consecutive duplicates of list elements.
    val a8 = Lists.p8.aggregate(List[Symbol]())((acc, x) =>  if (acc == Nil || acc.last != x) acc :+ x else acc, _ ++ _)
    //val a8 = Lists.q8.distinct
    println("P8: " + a8)

    // P9: Pack consecutive duplicates of list elements into sublists.
    val a9 = Lists.p8.aggregate(List[List[Symbol]]())((acc, x) => {
      if (acc == Nil) List(List(x))
      else if (acc.last.last == x)
        acc.dropRight(1) :+ (acc.last :+ x)
      else acc :+ List(x)
    }, _ ++ _)
    println("P9: " + a9)

    // P10: Run-length encoding of a list.
    val a10 = Lists.p8.aggregate(List[(Int, Symbol)]())((acc, x) => {
      if (acc == Nil) List((1, x))
      else if (acc.last._2 == x)
        acc.dropRight(1) :+ (acc.last._1 + 1, acc.last._2)
      else acc :+ (1, x)
    }, _ ++ _)
    println("P10: " + a10)

    // P11: Modified run-length encoding
    val a11 = a10.map {
      case (1, x: Symbol) => x
      case x => x
    }
    println("P11: " + a11)

    // P12: Decode a run-length encoded list.
    val a12 = a10.flatMap {
      case (n: Int, x) => List.fill(n)(x)
    }
    println("P12: " + a12)

    // P13: Run-length encoding of a list (direct solution).
    val a13 = Lists.p8.foldRight(List[(Int, Symbol)]())((x, acc) => {
      if (acc == Nil) List((1, x))
      else if (acc.head._2 == x) (acc.head._1 + 1, x) +: acc.drop(1)
      else (1, x) +: acc
    })
    println("P13: " + a13)

    // P14: Duplicate the elements of a list.
    val a14 = Lists.p14.flatMap(List.fill(2)(_))
    println("P14: " + a14)

    // P15: Duplicate the elements of a list a given number of times.
    val a15 = Lists.p14.duplicate(k)
    //val a15 = WithOption.duplicate(Lists.p14, k)
    println("P15: " + a15)
  }
}