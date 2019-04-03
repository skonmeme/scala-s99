package com.skt.skon.scala99.datastructure

sealed trait BinaryTree[+T] {
  def mirror: BinaryTree[T]
  def mirrorOfMirror: BinaryTree[T]
  def isSymmetry: Boolean
}

case class BinaryTreeLeaf[T](value: Option[T]) extends BinaryTree[T] {
  def mirror: BinaryTree[T] = this
  def mirrorOfMirror: BinaryTree[T] = this
  def isSymmetry: Boolean = true
}

case class BinaryTreeBranch[T](left: BinaryTree[T], right: BinaryTree[T]) extends BinaryTree[T] {
  def mirror: BinaryTree[T] = {
    this match {
      case BinaryTreeBranch(left: BinaryTreeBranch[T], right: BinaryTreeBranch[T]) =>  BinaryTreeBranch(right.mirror, left.mirror)
      case BinaryTreeBranch(left: BinaryTreeBranch[T], right: BinaryTreeLeaf[T]) =>  {
        val rightValue: Option[T] = if (right.value.isDefined) Some(null.asInstanceOf[T]) else None
        BinaryTreeBranch(BinaryTreeLeaf(rightValue), left.mirror)
      }
      case BinaryTreeBranch(left: BinaryTreeLeaf[T], right: BinaryTreeBranch[T]) =>  {
        val leftValue: Option[T] = if (left.value.isDefined) Some(null.asInstanceOf[T]) else None
        BinaryTreeBranch(right.mirror, BinaryTreeLeaf(leftValue))
      }
      case BinaryTreeBranch(left: BinaryTreeLeaf[T], right: BinaryTreeLeaf[T]) =>  {
        val leftValue: Option[T] = if (left.value.isDefined) Some(null.asInstanceOf[T]) else None
        val rightValue: Option[T] = if (right.value.isDefined) Some(null.asInstanceOf[T]) else None
        BinaryTreeBranch(BinaryTreeLeaf(rightValue), BinaryTreeLeaf(leftValue))
      }
    }
  }

  def mirrorOfMirror: BinaryTree[T] = {
    this match {
      case BinaryTreeBranch(left: BinaryTreeBranch[T], right: BinaryTreeBranch[T]) =>  BinaryTreeBranch(left.mirrorOfMirror, right.mirrorOfMirror)
      case BinaryTreeBranch(left: BinaryTreeBranch[T], right: BinaryTreeLeaf[T]) =>  {
        val rightValue: Option[T] = if (right.value.isDefined) Some(null.asInstanceOf[T]) else None
        BinaryTreeBranch(left.mirrorOfMirror, BinaryTreeLeaf(rightValue))
      }
      case BinaryTreeBranch(left: BinaryTreeLeaf[T], right: BinaryTreeBranch[T]) =>  {
        val leftValue: Option[T] = if (left.value.isDefined) Some(null.asInstanceOf[T]) else None
        BinaryTreeBranch( BinaryTreeLeaf(leftValue), right.mirrorOfMirror)
      }
      case BinaryTreeBranch(left: BinaryTreeLeaf[T], right: BinaryTreeLeaf[T]) =>  {
        val leftValue: Option[T] = if (left.value.isDefined) Some(null.asInstanceOf[T]) else None
        val rightValue: Option[T] = if (right.value.isDefined) Some(null.asInstanceOf[T]) else None
        BinaryTreeBranch(BinaryTreeLeaf(leftValue), BinaryTreeLeaf(rightValue))
      }
    }
  }

  def isSymmetry: Boolean = {
    left.mirrorOfMirror == right.mirror
  }
}

object BinaryTree {

  private def builder[T](trees: List[BinaryTree[T]]): List[BinaryTree[T]] = {
    val connected = trees.grouped(2).map(x => BinaryTreeBranch(x.head, x.last)).toList
    if (connected.length > 1) builder(connected)
    else connected
  }

  def perfect[T](level: Int, value: T): BinaryTree[T] = {
    if (level == 0) {
      BinaryTreeLeaf(Some(value))
    } else {
      val nodes = Math.pow(2, level).toInt
      builder(List.fill(nodes)(BinaryTreeLeaf(Some(value)))).head
    }
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