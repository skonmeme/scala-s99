package com.skt.skon.scala99.datatypes

import com.skt.skon.scala99.datastructure.BinarySearchTree

class ListExtention[T](list: List[T]) {

  def applyWithOption(k: Int): Option[T] = {
    if (k < 0 || k >= list.length) None
    else Some(list.apply(k))
  }

  def duplicate(k: Int): Option[List[T]] = {
    if (k < 0) None
    else Some(list.flatMap(List.fill(k)(_)))
  }

  def unique: List[T] = {
    def loop(set: Set[T], l: List[T]): List[T] = l match {
      case head :: tail if set contains head => loop(set, tail)
      case head :: tail => head :: loop(set + head, tail)
      case Nil => Nil
    }
    loop(Set(), list)
  }

  private def rearrangementCombinations(n: Int, c: List[Int]): List[Int] = {
    var length = n
    var rc = c
    var combination = List[Int]()

    while (length > 0) {
      if (rc == Nil || length - rc.head <= 0) {
        combination :+= length
        length = 0
      } else {
        combination :+= rc.head
        length -= rc.head
        rc = rc.drop(1)
      }
    }
    combination
  }

  // Very inefficient
  def combinationsWithReplacement(c: List[Int]): List[List[List[T]]] = {
    val nc = if (c.sum != c.length) rearrangementCombinations(list.length, c) else c

    val comb = list.permutations
    val ecomb = comb.map(l => {
      var nl = l
      var newList = List[List[T]]()
      for (i <- nc) {
        newList :+= nl.take(i).sortBy(_.toString)
        nl = nl.drop(i)
      }
      newList
    })
    ecomb.toList.distinct
  }

  def toBinarySearchTree(implicit order: Ordering[T]): BinarySearchTree[T] = {
    val tree = BinarySearchTree.of[T]
    list.foreach(tree.add(_))
    tree
  }

}

