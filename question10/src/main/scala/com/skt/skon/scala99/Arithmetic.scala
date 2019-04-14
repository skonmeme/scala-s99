package com.skt.skon.scala99

import com.skt.skon.scala99.datatypes.LongExtention

object Arithmetic {

  implicit def longToLong(x: Long): LongExtention = new LongExtention(x)

  def gcd(x: Long, y: Long): Option[Long] = {
    val divider = Math.min(x, y).divider
    if (x <= 0 || y <= 0) None
    else divider.reverse.find(d => x % d == 0 && y % d == 0)
  }

  def problem: Unit = {
    // P31: Determine whether a given integer number is prime.
    val t31: Long = 7L
    val a31 = t31.isPrime
    println("P31: " + a31)

    // P32: Determine the greatest common divisor of two positive integer numbers.
    //val t32 = (36L, 63L)
    val t32 = (5L, 10L)
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

    // P36: Determine the prime factors of a given positive integer (2).
    val a36 = a35.groupBy(_.toLong).toList.map(x => (x._1, x._2.length))
    // Map is represented by Tuple
    val a36_1 = t35.factorize
    println("P36: " + a36)

    // P37: Calculate Euler's totient function phi(m) (improved).
    val a37 = t34.etotient
    println("P37: " + a37)

    // P38: Compare the two methods of calculating Euler's totient function.
    val t38 = 10090L
    val t38_0 = System.nanoTime
    t38.totient
    val t38_1 = System.nanoTime
    t38.etotient
    val t38_2 = System.nanoTime
    val a38 = ((t38_1 - t38_0) / 1e9, (t38_2 - t38_1) / 1e9)
    println("P38: " + a38)

    // P39: A list of prime numbers.
    val t39 = (7L, 31L)
    val a39 = (t39._1 to t39._2).filter(_.isPrime).toList
    println("P39: " + a39)

    // P40: Goldbach's conjecture.
    val t40 = 28
    val a40 = t40.goldbach
    println("P40: " + a40)

    // P41: A list of Goldbach compositions.
    val t41 = (9, 20)
    val a41 = ((t41._1 / 2 + 1) * 2 to (t41._2 / 2) * 2 by 2).map(_.goldbach).toList
    println("P41: " + a41)
    val t41_1 = (3, 3000)
    val t41_2 = 50
    val a41_1 = ((t41_1._1 / 2 + 1) * 2 to (t41_1._2 / 2) * 2 by 2).map(_.goldbach).filter(x => x.get.exists(y => y._1 > t41_2))
    println("P41-1: " + a41_1.length)
  }

  def main(args: Array[String]): Unit = {
    problem
  }

}
