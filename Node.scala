package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer

abstract class Node {
  def emptyNode: Boolean
  def getChild(features : FeatureList): Node
  def getNodePrediction: Double = 0.0
  def getPrediction(features : FeatureList): Double = 0.0
  def insertLeft(node: Node): Unit = {}
  def insertRight(node: Node): Unit = {}
  def isLeaf: Boolean
}

class EmptyNode extends Node {
  override def emptyNode: Boolean = true
  override def isLeaf: Boolean = true
  override def getChild(features : FeatureList): Node = new EmptyNode
}

abstract class DataNode extends Node {
  var left: Node = null
  var right: Node = null
  val featureIndex: Int = -1
  val prediction: Double = 0.0
  
  override def emptyNode: Boolean = false
  override def getChild(features: FeatureList): Node
  override def getNodePrediction: Double = prediction
  override def getPrediction(features : FeatureList): Double = {
    var pred = getNodePrediction
    var child = getChild(features)
    while (!child.emptyNode) {
      pred = child.getNodePrediction
      child = child.getChild(features)
    }
    pred
  }
  override def insertLeft(node: Node): Unit = { left = node }
  override def insertRight(node: Node): Unit = { right = node }
  override def isLeaf: Boolean = left == null && right == null
}

class OrderedNode(val splitValue: FeatureValue) extends DataNode {
  override def getChild(features: FeatureList): Node = {
    if (isLeaf ||
        features.featureValues.length <= featureIndex) new EmptyNode
    if (features.featureValues(featureIndex) < splitValue) left
    right
  }
}

class CategoricalNode(values: ArrayBuffer[FeatureValue]) extends DataNode {
  val categories: Set[FeatureValue] = values.toSet[FeatureValue]
  
  override def getChild(features: FeatureList): Node = {
    if (isLeaf ||
        features.featureValues.length <= featureIndex) new EmptyNode
    if (categories.contains(features.featureValues(featureIndex))) left
    right
  }
}