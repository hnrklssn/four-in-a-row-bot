package com.hnrklssn.fourinarow.riddles

import scala.collection.mutable

/**
  *
  * @param matrix Dim1: layer, Dim2: node, Dim3: weights to connecting nodes
  * @param activationFunction The activation function (duh). Introduces non-linearity.
  */
class DependencyFreeNet(matrix: Vector[Vector[Vector[Double]]], activationFunction: Double => Double) {

  def apply(input: Vector[Double]): Vector[Double] = {
    val cache = mutable.Map[(Int, Int), Double]()
    matrix.head.indices
      .map(t => calc(t -> 0, cache, matrix :+ input.map(Vector(_)))).toVector
  }

  def calc(curr: (Int, Int), cache: mutable.Map[(Int, Int), Double], weights: Vector[Vector[Vector[Double]]]): Double = {
    if(curr._1 == weights.length - 1) {
      weights(curr._1)(curr._2).head
    } else {
      val nextLayer = curr._1 + 1
      val nodeConnectionWeights = weights(curr._1)(curr._2)
      val sumProduct = weights(nextLayer).indices
        .map(nextLayer -> _)
        .map(pos => cache.getOrElseUpdate(pos, calc(pos, cache, weights)))
        .zip(nodeConnectionWeights)
        .map(t => t._1 * t._2)
        .sum
      activationFunction(sumProduct)
    }
  }
}

object DependencyFreeNet {

  def parseWeights(json: String): Vector[Vector[Vector[Double]]] = {
    json.split("\\},\\{")
      .map(parseLayer)
      .toVector
      .reverse
  }

  def parseLayer(json: String): Vector[Vector[Double]] = {
    val split = json.split("[,\\[\\]\\{\\}:\"\\s]+")
    val rowIndex = split.indexOf("rows")
    val numberOfRows = split(rowIndex + 1).toInt
    val colIndex = split.indexOf("cols")
    val numberOfCols = split(colIndex + 1).toInt
    val dataIndex = split.indexOf("data")
    split.slice(dataIndex + 1, dataIndex + numberOfRows * numberOfCols + 1)
      .map(_.toDouble)
      .toVector
      .grouped(numberOfRows)
      .toVector
  }

  def apply(weights: String, activationFunction: Double => Double): DependencyFreeNet = new DependencyFreeNet(parseWeights(weights), activationFunction)
}
