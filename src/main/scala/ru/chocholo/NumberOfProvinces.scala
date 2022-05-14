package ru.chocholo

import scala.annotation.tailrec

object NumberOfProvinces {

  def findCircleNum(isConnected: Array[Array[Int]]): Int = {
    val resultArray = isConnected.indices.toArray

    @tailrec
    def find(x: Int): Int = if (x != resultArray(x)) find(resultArray(x)) else x

    def union(x: Int, y: Int): Unit = {
      val rootX = find(x)
      val rootY = find(y)
      if (rootX != rootY) resultArray(rootY) = rootX
    }

    for {
      rowIndex <- isConnected.indices
      columnIndex <- isConnected.indices
      if isConnected(rowIndex)(columnIndex) == 1 && rowIndex != columnIndex
    } yield union(rowIndex, columnIndex)
    resultArray.map(find).distinct.length
  }

  def main(args: Array[String]): Unit = {
    println(findCircleNum(Array(Array(1,1,0), Array(1,1,0), Array(0,0,1)))) // 2
    println(findCircleNum(Array(Array(1,0,0), Array(0,1,0), Array(0,0,1)))) // 3
    println(findCircleNum(Array(Array(1,1,1), Array(1,1,1), Array(1,1,1)))) // 1
    println(findCircleNum(Array(Array(1,0,0), Array(0,1,1), Array(0,1,1)))) //2
    println(findCircleNum(Array(Array(1,0,0,1), Array(0,1,0,0), Array(0,0,1,0), Array(1,0,0,1)))) // 3
    println(findCircleNum(Array(Array(1,0,0,1), Array(0,1,1,0), Array(0,1,1,1), Array(1,0,1,1)))) //1
  }

}
