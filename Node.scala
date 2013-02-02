package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer

abstract class Node {
  def emptyNode: Boolean
  def getChild(features : FeatureList): Node
  def getLeftChild: Node
  def getRightChild: Node
  def getId: Int = -1
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
  override def getLeftChild: Node = new EmptyNode
  override def getRightChild: Node = new EmptyNode
}

abstract class DataNode extends Node {
  val id: Int
  var left: Node = new EmptyNode
  var right: Node = new EmptyNode
  val featureIndex: Int = -1
  val prediction: Double = 0.0
  
  override def emptyNode: Boolean = false
  override def getChild(features: FeatureList): Node
  override def getLeftChild: Node = left
  override def getRightChild: Node = right
  override def getId = id
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
  override def isLeaf: Boolean = left.emptyNode && right.emptyNode
}

class OrderedNode(val id: Int,
    val splitValue: FeatureValue) extends DataNode {
  override def getChild(features: FeatureList): Node = {
    if (features.featureValues.length <= featureIndex) new EmptyNode
    else if (isLeaf) this
    else if (features.featureValues(featureIndex) < splitValue) left
    else right
  }
}

class CategoricalNode(val id: Int,
    val values: ArrayBuffer[FeatureValue]) extends DataNode {
  val categories: Set[FeatureValue] = values.toSet[FeatureValue]
  
  override def getChild(features: FeatureList): Node = {
	if (features.featureValues.length <= featureIndex) new EmptyNode
    else if (isLeaf) this
    if (categories.contains(features.featureValues(featureIndex))) left
    else right
  }
}