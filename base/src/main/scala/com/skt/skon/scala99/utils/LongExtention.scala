package com.skt.skon.scala99.utils

class LongExtention(x: Long) {

  def prime: Boolean = {
    x match {
      case _ if x <= 1 => false
      case 2 => true
      case _ => ! (2L to x / 2 + 1).exists(x % _ == 0)
    }
  }

  def divider: List[Long] = {
    x match {
      case _ if x <= 1 => List(1)
      case _ => (1L to x / 2 + 1).filter(x % _ == 0).toList
    }
  }

  def dividers: List[Long] = {
    val d = divider.tail.iterator
    var z = x
    var l = List[Long]()
    d.foreach { dd =>
      while (z % dd == 0) {
        l :+= dd
        z /= dd
      }
    }
    l
  }

  def gcd(y: Long): Option[Long] = {
    val d = new LongExtention(Math.min(x, y)).divider
    if (x <= 0 || y <= 0) None
    else d.reverse.find(d => x % d == 0 && y % d == 0)
  }

  def totient: Long = {
    (x - 1 to 2L by -1).count(gcd(_).contains(1))
  }

}
