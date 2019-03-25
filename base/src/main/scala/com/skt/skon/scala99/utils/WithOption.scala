package com.skt.skon.scala99.utils

object WithOption {

  def listApply[T](list: List[T], k: Int): Option[T] = {
    if (k < 0 || k >= list.length) None
    else Some(list.apply(k))
  }

}
