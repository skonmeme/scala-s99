package com.skt.skon.scala99

import com.skt.skon.scala99.datasets.Lists
import com.skt.skon.scala99.datatypes.BooleanExtention
import com.skt.skon.scala99.utils.GenericFunction

object Logics {

  implicit def booleanToBooleanExtention(x: Boolean): BooleanExtention = new BooleanExtention(x)

  def problem: Unit = {
    // P46: Truth tables for logical expressions.
    // and, or, nand, nor, xor, impl(logical implication), and equ(equal)
    val a46_and  = GenericFunction.logicTable((x, y) => x && y)
    val a46_or   = GenericFunction.logicTable((x, y) => x || y)
    val a46_nand = GenericFunction.logicTable((x, y) => !(x && y))
    val a46_nor  = GenericFunction.logicTable((x, y) => !(x || y))
    val a46_xor  = GenericFunction.logicTable((x, y) => x ^ y)
    val a46_impl = GenericFunction.logicTable((x, y) => !x || y)
    val a46_equ  = GenericFunction.logicTable((x, y) => x == y)
    println("P46 and  : " + a46_and)
    println("P46 or   : " + a46_or)
    println("P46 nand : " + a46_nand)
    println("P46 nor  : " + a46_nor)
    println("P46 xor  : " + a46_xor)
    println("P46 impl : " + a46_impl)
    println("P46 equ  : " + a46_equ)
    val a46 = GenericFunction.logicTable((x, y) => x && (x || y))
    println("P46      : " + a46)

    // P47: Truth tables for logical expressions (2).
    val a47 = GenericFunction.logicTable((x, y) => x and (x or !y))
    println("P47 : " + a47)

    // P48
    // P49: Gray code
    val a49 = GenericFunction.gray(4)
    println("P49 : " + a49)

    // P50: Huffman code
    val a50 = GenericFunction.huffman(Lists.p50)
    println("P50 : " + a50)
  }

  def main(args: Array[String]): Unit = {
    problem
  }

}
