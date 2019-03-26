package com.skt.skon.scala99.implicits

class ListExtention[T](list: List[T]) {

  def applyWithOption(k: Int): Option[T] = {
    if (k < 0 || k >= list.length) None
    else Some(list.apply(k))
  }

  def duplicate(k: Int): Option[List[T]] = {
    if (k < 0) None
    else Some(list.flatMap(List.fill(k)(_)))
  }

}

