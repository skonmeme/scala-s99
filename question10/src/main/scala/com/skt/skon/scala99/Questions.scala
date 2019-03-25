package com.skt.skon.scala99

import com.skt.skon.scala99.datasets.Lists
import com.skt.skon.scala99.utils.{WithOption, WithTag}

object Questions {
  def main(args: Array[String]): Unit = {
    // Q1: Find the last element of a list.
    val a1 = Lists.q1.last
    // List.q1.head
    // List.q1.tail
    //println(a1)

    // Q2: Find the last but one element of a list.
    val a2 = Lists.q1.reverse.apply(1)
    // Lists.q1.reverse.iterator
    //println(a2)

    // Q3: Find the Kth element of a list.
    val k = 3
    val a3 = WithOption.listApply(Lists.q1, k)
    //a3 match {
    //  case None => println("Out of range")
    //  case _ => println(a3)
    //}

    // Q4: Find the number of elements of a list.
    val a4 = Lists.q1.length

    // Q5: Reverse a list.
    val a5 = Lists.q1.reverse

    // Q6: Find out whether a list is a palindrome.
    val a6 = (Lists.q6 == Lists.q6.reverse)
    //println(a6)

    // Q7: Flatten a nested list structure.
    val a7 = WithTag.flatMap[Any](Lists.q7)
    //println(a7)

    // Q8: Eliminate consecutive duplicates of list elements.
    val a8 = Lists.q8.aggregate(List[Symbol]())((acc, x) =>  if (acc == Nil || acc.last != x) acc :+ x else acc, _ ++ _)
    //val a8 = Lists.q8.distinct
    //println(a8)

    // Q9: Pack consecutive duplicates of list elements into sublists.
    val a9 = Lists.q8.aggregate(List[List[Symbol]]())((acc, x) => {
      if (acc == Nil) List(List(x))
      else if (acc.last.last == x)
        acc.dropRight(1) :+ (acc.last :+ x)
      else acc :+ List(x)
    }, _ ++ _)
    //println(a9)

    // Q10: Run-length encoding of a list.
    val a10 = Lists.q8.aggregate(List[(Int, Symbol)]())((acc, x) => {
      if (acc == Nil) List((1, x))
      else if (acc.last._2 == x)
        acc.dropRight(1) :+ (acc.last._1 + 1, acc.last._2)
      else acc :+ (1, x)
    }, _ ++ _)
    println(a10)
  }
}