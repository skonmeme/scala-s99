package com.skt.skon.scala99

import com.skt.skon.scala99.implicits.ListExtention
import com.skt.skon.scala99.utils.LongExtention

object Arithmetic {

  implicit def longToLong(x: Long): LongExtention = new LongExtention(x)

  def gcd(x: Long, y: Long): Option[Long] = {
    val divider = Math.min(x, y).divider
    if (x <= 0 || y <= 0) None
    else divider.reverse.find(d => x % d == 0 && y % d == 0)
  }

  def main(args: Array[String]): Unit = {
    // P31: Determine whether a given integer number is prime.
    val t31: Long = 7L
    val a31 = t31.prime
    println("P31: " + a31)

    // P32: Determine the greatest common divisor of two positive integer numbers.
    val t32 = (36L, 63L)
    val a32 = gcd(t32._1, t32._2)
    println("P32: " + a32)

    // P33: Determine whether two positive integer numbers are coprime.
    val t33 = (35L, 64L)
    val a33 = gcd(t33._1, t33._2) match {
      case Some(1) => true
      case _ => false
    }
    println("P33: " + a33)

    // P34: Calculate Euler's totient function phi(m).
    val t34 = 10L
    val a34 = t34.totient
    //val a34 = (t31 - 1 to 1 by -1).filter(gcd(_, t31).contains(1))
    println("P34: " + a34)

    // P35: Determine the prime factors of a given positive integer.
    val t35 = 315L
    val a35 = t35.dividers
    println("P35: " + a35)
  }

}
