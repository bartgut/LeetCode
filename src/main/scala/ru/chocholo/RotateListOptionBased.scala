package ru.chocholo

import cats.effect.IO
import cats.syntax.traverse._

/*
  https://leetcode.com/problems/rotate-list/
 */
class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}
object RotateList {

  def getLast(start: ListNode): Option[(ListNode, Int)] = {
    def getLastInternal(curr: (ListNode, Int)): (ListNode, Int) = {
      Option(curr._1.next)
        .map((_,  curr._2+1))
        .map(getLastInternal)
        .getOrElse(curr)
    }
    Option(start)
      .map((_, 1))
      .map(getLastInternal)
  }

  def createNew(startNode: ListNode, lastNode: ListNode, position: Int): ListNode = {
    Option(position)
      .filter(_ != 0)
      .map(p => {
        lastNode.next = startNode
        createNew(startNode.next, lastNode.next, p-1)
      })
      .getOrElse({
        lastNode.next = null
        startNode
      })
  }

  def rotateRight(head: ListNode, k: Int): ListNode = {
    getLast(head).map { last =>
      val (lastNode, size) = last
      val lastInNewPosition = size - (k % size)
      createNew(head, lastNode, lastInNewPosition)
    }.getOrElse(head)
  }

  def print(start: ListNode): IO[Unit] = {
    Option.apply(start)
      .traverse(node => IO(println(node.x)).as(start))
      .flatMap(p => p.traverse(x => print(x.next)).as(()))
  }

  def main(args: Array[String]): Unit = {
    val c = new ListNode(3, null)
    val b = new ListNode(2, c)
    val a = new ListNode(1, b)
    print(rotateRight(a, 2)).unsafeRunSync()

  }
}
