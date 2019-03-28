package com.skt.skon.scala99

import com.skt.skon.scala99.datasets.Lists
import com.skt.skon.scala99.implicits.ListExtention
import com.skt.skon.scala99.utils.{GenericFunction, WithTag}

import scala.util.Random

object Arrays {

  implicit def listToListExtention[T](list: List[T]): ListExtention[T] = new ListExtention(list)

  def problem: Unit = {
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
    val t3 = 3
    //val a3 = WithOption.listApply(Lists.p1, t3)
    val a3 = Lists.p1.applyWithOption(t3)
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
    val t14 = 2
    val a14 = Lists.p14.flatMap(List.fill(t14)(_))
    println("P14: " + a14)

    // P15: Duplicate the elements of a list a given number of times.
    val t15 = 3
    val a15 = Lists.p14.duplicate(t15)
    //val a15 = WithOption.duplicate(Lists.p14, t15)
    println("P15: " + a15)

    // P16: Drop every Nth element from a list.
    val t16 = 3
    val a16 = Lists.p16.grouped(t16).flatMap(x => x.dropRight(1)).toList
    println("P16: " + a16)

    // P17: Split a list into two parts.
    val t17 = 3
    val a17 = Lists.p16.splitAt(t17)
    println("P17: " + a17)

    // P18: Extract a slice from a list
    val t18 = (3, 7)
    val a18 = Lists.p16.slice(t18._1, t18._2)
    println("P18: " + a18)

    // P19: Rotate a list N places to the left.
    val a19 = a17._2 ++ a17._1
    println("P19: " + a19)

    // P20: Remove the Kth element from a list.
    val a20 = (a17._1.dropRight(1) ++ a17._2, a17._1.last)
    println("P20: " + a20)

    // P21: Insert an element at a given position into a list.
    val t21 = 'new
    val a21 = (a17._1 :+ t21) ++ a17._2
    println("P21: " + a21)

    // P22: Create a list containing all integers within a given range.
    val t22 = (4, 9)
    val a22 = (t22._1 to t22._2).toList
    println("P22: " + a22)

    // P23: Extract a given number of randomly selected elements from a list.
    // with replacement
    val t23 = Random
    val t23_n = Lists.p16.length
    val a23 = (1 to 3).map(x => Lists.p16.apply(t23.nextInt(t23_n))).toList
    println("P23: " + a23)

    // P24: Lotto: Draw N different random numbers from the set 1..M.
    // without replacement
    val t24 = (6, 45)
    val a24 = Random.shuffle((1 to t24._2).toList).take(t24._1)
    println("P24: " + a24)

    // P25: Generate a random permutation of the elements of a list.
    val a25 = Random.shuffle(Lists.p16)
    println("P25: " + a25)

    // P26: Generate the combinations of K distinct objects chosen from the N elements of a list.
    val a26 = Lists.p16.combinations(t17).toList
    println("P26: " + a26)

    // P27: Group the elements of a set into disjoint subsets.
    val a27 = Lists.p27.combinationsWithReplacement(List(3, 2, 2))
    println("P27: " + a27)

    // P28: Sorting a list of lists according to length of sublists.
    val a28 = Lists.p28.sortBy(x => x.length)
    println("P28: " + a28)
  }

  def main(args: Array[String]): Unit = {
    problem
  }

}