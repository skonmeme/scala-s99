package com.skt.skon.scala99.datastructure

import java.util.NoSuchElementException

import util.control.Breaks._

class EightQueens(plateSize: Int, nQueens: Int) extends Iterator[List[(Int, Int)]] {

  class Block(from: Int, to: Int) extends Iterator[(Int, Int)] {
    private var position: (Int, Int) = (from, 0)

    override def hasNext: Boolean = if (position._1 == to && position._2 == plateSize) false else true

    @throws(classOf[NoSuchElementException])
    override def next: (Int, Int) = {
      val newPosition = if (position._2 < plateSize) (position._1, position._2 + 1)
      else if (position._1 < plateSize) (position._1 + 1, 0)
      else throw new NoSuchElementException
      //else (0, 0)
      //if (pp == (0, 0)) throw new NoSuchElementException
      position = newPosition
      position
    }
  }

  private var pool: List[Block] = Nil
  private var queens: List[(Int, Int)] = Nil
  private var buffer: Option[List[(Int, Int)]] = None

  @throws(classOf[NoSuchElementException])
  override def hasNext: Boolean = {
    pool == Nil || buffer.isDefined || {
      try {
        buffer = Some(next)
        true
      } catch {
        case e: NoSuchElementException => false
      }
    }
  }

  def isCollisionWith(queen: (Int, Int)): Boolean = {
    if (queens.isEmpty) false
    else queens.exists({
      case (x: Int, y: Int) if x == queen._1 || y == queen._2 => true
      case (x: Int, y: Int) if (x - y) == (queen._1 - queen._2) || (x - y) == -(queen._1 - queen._2) => true
      case _ => false
    })
  }

  def getNext(order: Int): (Int, Int) = {
    var queen = (0, 0)
    do {
      queen = pool.apply(order).next
    } while (isCollisionWith(queen))
    queen
  }

  @throws(classOf[NoSuchElementException])
  override def next: List[(Int, Int)] = {
    if (pool == Nil) (1 to nQueens).foreach(x => pool :+= new Block(x, plateSize - nQueens + x))

    if (buffer.isDefined) {
      val q = buffer.get
      buffer = None
      q
    }
    else {
      breakable {
        var order = 0
        if (queens.length == nQueens) {
          queens = queens.drop(1)
        } else if (queens.length < nQueens - 1) {
          order = nQueens - queens.length - 1
        } // if (queens.length == nQueens - 1) order = 0 with no drop
        while (true) {
          try {
            queens +:= getNext(order)
            if (order == 0) break
            else {
              order -= 1
              pool = pool.updated(order, new Block(order + 1, plateSize - nQueens + order + 1))
            }
          } catch {
            case e: NoSuchElementException =>
              queens = queens.drop(1)
              order += 1
              if (order >= nQueens) throw new NoSuchElementException("no more queens")
          }
        }
      }
      queens
    }
  }

}
