package com.skt.skon.scala99.datatypes

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
      case _ => {
        val d = if (x / 2 + 1 == x) (1L to x / 2 + 1).toList
        else (1L to x / 2 + 1).toList :+ x
        d.filter(x % _ == 0)
      }
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
    (x - 1 to 1L by -1).count(gcd(_).contains(1))
  }

  def factorize: Map[Long, Int] = {
    dividers.groupBy(_.toLong).map(z => (z._1, z._2.length))
  }

  def pow(k: Int): Long = {
    k match {
      case _ if k < 0 => 0L
      case _ if k <= 0 => 1L
      case _ => List.fill(k)(x).product
    }
  }

  def etotient: Long = {
    factorize.map(z => new LongExtention(z._1).pow(z._2 - 1) * (z._1 - 1)).product
  }

  def goldbach: Option[List[(Long, Long)]] = {
    if (x < 4 || x % 2 == 0) {
      val p = (1L to x).filter(new LongExtention(_).prime)
      Some(p.filter(z => p.filter(y => y >= z).contains(x - z)).map(z => (z, x - z)).toList)
    } else None
  }

}
