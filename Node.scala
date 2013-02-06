package gradientBoostedTree

//import scala.collection.mutable.ArrayBuffer

abstract class Node {
  def getLeaf(features : Array[FeatureValue]): Node
  def getLeftChild: Node
  def getRightChild: Node
  def getId: Int = -1
  def getNodePrediction: Double = 0.0
  def getPrediction(features : Array[FeatureValue]): Double = 0.0
  def insertChildren(leftChild: Node,
      rightChild: Node,
      values: Array[FeatureValue]): Unit = {}
  def isEmptyNode: Boolean
  def isLeaf: Boolean = false
}

class EmptyNode extends Node {
  override def getLeaf(features : Array[FeatureValue]): Node = new EmptyNode
  override def getLeftChild: Node = new EmptyNode
  override def getRightChild: Node = new EmptyNode
  override def isEmptyNode: Boolean = true
}

abstract class DataNode extends Node {  
  override def getLeaf(features: Array[FeatureValue]): Node
  override def getLeftChild: Node = left
  override def getRightChild: Node = right
  override def getId = id
  override def getNodePrediction: Double = prediction
  // Given the array of feature values, find the prediction for the corresponding
  // leaf node.
  override def getPrediction(features : Array[FeatureValue]): Double = {
    if (isLeaf) getNodePrediction
    else {
      var pred = 0.0
      var child: Node = this
      do {
        child = child.getLeaf(features)
        pred = child.getNodePrediction
      }
      while (!child.isLeaf && !child.isEmptyNode)
      pred
    }
  }
  override def isEmptyNode = false
  override def isLeaf = left.isEmptyNode && right.isEmptyNode
  
  // **************************************************************************
  //
  // Protected
  //
  // **************************************************************************
  
  protected val id: Int
  protected var left: Node = new EmptyNode
  protected var right: Node = new EmptyNode
  protected val featureIndex: Int = -1
  protected val prediction: Double = 0.0
}

// A node for ordered data, e.g. value <= 1 and value > 1
// id: The id for the node
// featureIndex: The index into the array of FeatureValue that this splits.
// prediction: The prediction value, provided this is a leaf node.
class OrderedNode(val id: Int,
    override val featureIndex: Int,
    override val prediction: Double) extends DataNode {
  var splitValue: FeatureValue = new FeatureValue(0)

  // Return the child leaf node corresponding to the array of values.
  // All features are assumed present. If not enough features are provided
  // then return an EmptyNode.
  override def getLeaf(features: Array[FeatureValue]): Node = {
    if (isLeaf) this
    else if (features.length <= featureIndex) new EmptyNode
    else if (features(featureIndex) < splitValue) left
    else right
  }

  // Insert both left and right children and set the splitting value.
  override def insertChildren(leftChild: Node,
      rightChild: Node,
      values: Array[FeatureValue]): Unit = {
    left = leftChild
    right = rightChild
    splitValue = values(0)
  }
}

// A node for categorical data, e.g. for values in (1, 2, 3, 4)
// id: The id for the node
// featureIndex: The index into the array of FeatureValue that this splits.
// prediction: The prediction value, provided this is a leaf node.
class CategoricalNode(val id: Int,
    override val featureIndex: Int,
    override val prediction: Double) extends DataNode {
  var categories: Set[FeatureValue] = null
  
  // Return the child leaf node corresponding to the array of values.
  // All features are assumed present. If not enough features are provided
  // then return an EmptyNode.
  override def getLeaf(features: Array[FeatureValue]): Node = {
    if (isLeaf) this
    else if (features.length <= featureIndex) new EmptyNode
    else if (categories.contains(features(featureIndex))) left
    else right
  }
  
  // Insert both left and right children and set the splitting values.
  override def insertChildren(leftChild: Node,
      rightChild: Node,
      values: Array[FeatureValue]): Unit = {
    left = leftChild
    right = rightChild
    categories = values.toSet[FeatureValue]  
  }
}