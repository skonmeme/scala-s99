package com.skt.skon.scala99.implicits

class BooleanExtention(x: Boolean) {

  def and(y: Boolean): Boolean = x && y
  def or(y: Boolean): Boolean  = x || y
  def nand(y: Boolean): Boolean = !(x && y)
  def nor(y: Boolean): Boolean = !(x || y)
  def xor(y: Boolean): Boolean = x ^ y
  def impl(y: Boolean): Boolean = !x || y
  def equ(y: Boolean): Boolean = x == y

}
