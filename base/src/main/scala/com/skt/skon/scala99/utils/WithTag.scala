package com.skt.skon.scala99.utils

import scala.reflect.ClassTag

object WithTag {

  def flatMap[T: ClassTag](list: List[Any]): List[Any] = {
    list.flatMap {
      case aList: List[Any] => flatMap(aList)
      case element: T => Some(element)
      case _ => None
    }
  }

}
