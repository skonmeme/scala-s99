package com.skt.skon.scala99.datastructure

class BinarySearchTree[T](implicit order: Ordering[T]) {

  private var tree = Map[T, (Option[T], Option[T])]()
  private var headValue: T = _

  private def head: (T, (Option[T], Option[T])) = (headValue, tree.apply(headValue))
  private def node(root: T): (T, (Option[T], Option[T])) = (root, tree.apply(root))

  private def addToTree(n: (T, (Option[T], Option[T])), value: T): Unit = {
    val root  = n._1
    val left  = n._2._1
    val right = n._2._2
    order.compare(root, value) match {
      case 1 =>
        if (left.isEmpty) {
          tree += root -> (Some(value), right)
          tree += value -> (None, None)
        } else addToTree(node(left.get), value)
      case -1 =>
        if (right.isEmpty) {
          tree += root -> (left, Some(value))
          tree += value -> (None, None)
        }
        else addToTree(node(right.get), value)
      case _ =>
    }
  }

  def add(value: T): BinarySearchTree[T] = {
    if (tree.headOption.isEmpty) {
      tree += value -> (None, None)
      headValue = value
    }
    else addToTree(head, value)
    this
  }

  def :+(value: T): BinarySearchTree[T] = {
    val t = BinarySearchTree.of[T]
    t.add(value)
  }

  private def nodeToList(n: (T, (Option[T], Option[T]))): List[T] = {
    n match {
      case (root, (Some(left), Some(right))) => nodeToList(node(left)) ::: List(root) ::: nodeToList(node(right))
      case (root, (Some(left), None)) => nodeToList(node(left)) :+ root
      case (root, (None, Some(right))) => root +: nodeToList(node(right))
      case (root, (None, None)) => List(root)
    }
  }

  def nodeEquivalent(t: BinarySearchTree[T])(n1: (T, (Option[T], Option[T])), n2: (T, (Option[T], Option[T]))): Boolean = {
    val left  = {
      if (n1._2._1.isDefined && n2._2._1.isDefined) nodeEquivalent(t)(node(n1._2._1.get), t.node(n2._2._1.get))
      else if (n1._2._1.isEmpty && n2._2._1.isEmpty) true
      else false
    }
    val right = {
      if (n1._2._2.isDefined && n2._2._2.isDefined) nodeEquivalent(t)(node(n1._2._2.get), t.node(n2._2._2.get))
      else if (n1._2._1.isEmpty && n2._2._1.isEmpty) true
      else false
    }
    left && right
  }

  def isEquivalent(t: BinarySearchTree[T]): Boolean = {
    if (tree.size != t.tree.size) false

    val n1 = head
    val n2 = t.head
    nodeEquivalent(t)(n1, n2)
  }

  def isSymmetric: Boolean = {
    val r = reverse
    isEquivalent(r)
  }

  def toList: List[T] = nodeToList(head)

  def reverse: BinarySearchTree[T] = {
    var t = BinarySearchTree.of[T](Ordering[T].reverse)
    t.headValue = headValue
    tree.foreach(x => t.tree += x._1 -> (x._2._2, x._2._1))
    t
  }

  override def toString: String = tree.toString

}

object BinarySearchTree {
  def of[T](implicit order: Ordering[T]): BinarySearchTree[T] = {
    new BinarySearchTree[T]
  }
}


