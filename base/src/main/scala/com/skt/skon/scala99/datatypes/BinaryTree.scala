package com.skt.skon.scala99.datatypes

sealed trait BinaryTree[+T] {
  def mirror: BinaryTree[T]
  def isSymmetry: Boolean
}

case class BinaryTreeLeaf[T](value: Option[T]) extends BinaryTree[T] {
  def mirror: BinaryTree[T] = this
  def isSymmetry: Boolean = true
}

case class BinaryTreeBranch[T](left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T] {
  def mirror: BinaryTree[T] = {
    this
  }

  def isSymmetry: Boolean = {
    left == right.mirror
  }
}

object BinaryTree {

  private def builder[T](trees: List[BinaryTree[T]]): List[BinaryTree[T]] = {
    val connected = trees.grouped(2).map(x => BinaryTreeBranch(x.head, x.last)).toList
    if (connected.length > 1) builder(connected)
    else connected
  }

  def perfect[T](level: Int, value: T): BinaryTree[T] = {
    val nodes = Math.pow(2, level).toInt
    builder(List.fill(nodes)(BinaryTreeLeaf(Some(value)))).head
  }

  def complete[T](n: Int, value: T): BinaryTree[T] = {
    val level = (Math.log(n) / Math.log(2)).ceil.toInt
    val fullnode = Math.pow(2, level).toInt
    if (fullnode == n) perfect(level, value)
    else {
      val nodes = List.fill(n)(BinaryTreeLeaf(Some(value))) ::: List.fill(fullnode - n)(BinaryTreeLeaf[T](None))
      builder(nodes).head
    }
  }

}