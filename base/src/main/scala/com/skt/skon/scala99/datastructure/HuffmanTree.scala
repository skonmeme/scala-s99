package com.skt.skon.scala99.datastructure

sealed trait HuffmanTree[+T] {
  def code: List[(T, String)]
}

case class HuffmanLeaf[T](symbol: T, count: Int) extends HuffmanTree[T] {
  def code: List[(T, String)] = List((symbol, "0"))
}

case class HuffmanBranch[T](left: HuffmanTree[T], right: HuffmanTree[T], count: Int) extends HuffmanTree[T] {
  def code: List[(T, String)] = {
    val l = left match {
      case HuffmanLeaf(symbol, _) => List((symbol, "0"))
      case b@HuffmanBranch(_, _, _) => b.code.map(z => (z._1, z._2 + "0"))
    }
    val r = right match {
      case HuffmanLeaf(symbol, _) => List((symbol, "1"))
      case b@HuffmanBranch(_, _, _) => b.code.map(z => (z._1, "1" + z._2 ))
    }
    l ++ r
  }

  def add(leaf1: HuffmanLeaf[T], leaf2: HuffmanLeaf[T]): HuffmanBranch[T] = {
    if (count < leaf1.count) {
      HuffmanBranch(HuffmanBranch(this, leaf1, count + leaf1.count), leaf2, count + leaf1.count + leaf2.count)
    } else if (count < leaf2.count) {
      HuffmanBranch(HuffmanBranch(leaf1, this, leaf1.count + count), leaf2, leaf1.count + count + leaf2.count)
    } else {
      HuffmanBranch(HuffmanBranch(leaf1, leaf2, leaf1.count + leaf2.count), this, leaf1.count + leaf2.count + count)
    }
  }

  def add(leaf: HuffmanLeaf[T]): HuffmanBranch[T] = {
    if (count < leaf.count) {
      HuffmanBranch(this, leaf, this.count + leaf.count)
    } else {
      HuffmanBranch(leaf, this, leaf.count + this.count)
    }
  }
}

