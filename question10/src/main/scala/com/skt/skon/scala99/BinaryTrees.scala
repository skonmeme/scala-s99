package com.skt.skon.scala99

import com.skt.skon.scala99.datasets.Lists
import com.skt.skon.scala99.datastructure.{BinarySearchTree, BinaryTree}
import com.skt.skon.scala99.datatypes.ListExtention

object BinaryTrees {

  implicit def listToListExtention[T](list: List[T]): ListExtention[T] = new ListExtention(list)

  def problem: Unit = {
    // P55: Construct completely balanced binary trees.
    val a55 = BinaryTree.complete(7, 1)
    println("P55 : " + a55)

    // P56: Symmetric binary trees.
    val a56 = BinaryTree.complete(3, 1).isSymmetry
    println("P56 : " + a56)

    // P57: Binary
    val a57 = BinarySearchTree.of[Int]
    a57.add(2).add(3).add(0)
    println("P57 : " + a57)
    val a57_1 = Lists.p57.toBinarySearchTree
    println("P57-1 : " + a57_1.isSymmetric)
  }

  def main(args: Array[String]): Unit = {
    problem
  }

}
