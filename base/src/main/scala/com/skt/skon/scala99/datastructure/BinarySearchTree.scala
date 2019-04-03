package com.skt.skon.scala99.datastructure

class BinarySearchTree[T] {

  private var tree = Map[T, (Option[T], Option[T])]()

  private def node(root: T): (T, (Option[T], Option[T])) = (root, tree.apply(root))

  private def addToTree(n: (T, (Option[T], Option[T])), value: T)(implicit order: Ordering[T]): Unit = {
    val root  = n._1
    val left  = n._2._1
    val right = n._2._2
    order.compare(root, value) match {
      case 1 =>
        if (left.isEmpty) {
          tree += root -> (Some(value), right)
          tree += value -> (None, None)
        } else addToTree(node(left.get), value)(order)
      case -1 =>
        if (right.isEmpty) {
          tree += root -> (left, Some(value))
          tree += value -> (None, None)
        }
        else addToTree(node(right.get), value)(order)
      case _ =>
    }
  }

  def add(value: T)(implicit order: Ordering[T]): BinarySearchTree[T] = {
    if (tree.headOption.isEmpty) tree += value -> (None, None)
    else addToTree(tree.head, value)(order)
    this
  }

  def :+(value: T)(implicit order: Ordering[T]): BinarySearchTree[T] = {
    val t = BinarySearchTree.of[T]
    t.add(value)
  }

  def nodeToList(n: (T, (Option[T], Option[T]))): List[T] = {
    n match {
      case (root, (Some(left), Some(right))) => nodeToList(node(left)) ::: List(root) ::: nodeToList(node(right))
      case (root, (Some(left), None)) => nodeToList(node(left)) :+ root
      case (root, (None, Some(right))) => root +: nodeToList(node(right))
      case (root, (None, None)) => List(root)
    }
  }

  def isSymmetric: Boolean = {

  }

  def toList: List[T] = nodeToList(tree.head)

  override def toString: String = tree.toString

}

object BinarySearchTree {
  def of[T]: BinarySearchTree[T] = {
    new BinarySearchTree[T]
  }
}


