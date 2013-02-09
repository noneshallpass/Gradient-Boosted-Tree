package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer
import scala.reflect.BeanProperty

/*abstract class Node {
  def getId: Int = -1
  def getFeatureIndex: Int = -1
  def getLeaf(features : Array[FeatureValue]): Node
  def getLeftChild: Node
  def getRightChild: Node
  def setLeftChild(node: Node): Unit = {}
  def setRightChild(node: Node): Unit = {}
  def getParent: Node = new EmptyNode
  def getNodePrediction: Double = 0.0
  def getPrediction(features : Array[FeatureValue]): Double = 0.0
  def insertChildren(leftChild: Node,
      rightChild: Node,
      values: ArrayBuffer[FeatureValue]): Unit = {}
  def isEmptyNode: Boolean
  def isLeaf: Boolean = false
  def replaceNode(newNode: Node): Unit = {}
}*/

class EmptyNode extends Node {
  override def getLeaf(features : Array[FeatureValue]): Node = this
  override def getLeftChild: Node = this
  override def getRightChild: Node = this
  override def isEmptyNode: Boolean = true
}
object EmptyNode {
  def getEmptyNode: Node = emptyNode
  val emptyNode: EmptyNode = new EmptyNode
}

abstract class Node {  
  def getId = id
  def getFeatureIndex = featureIndex
  def getLeaf(features: Array[FeatureValue]): Node = EmptyNode.getEmptyNode
  def getNodePrediction: Double = prediction
  
  // Given the array of feature values, find the prediction for the corresponding
  // leaf node.
  def getPrediction(features : Array[FeatureValue]): Double = {
    if (isLeaf) getNodePrediction
    else {
      var pred = 0.0
      var child: Node = this
      do {
        child = child.getLeaf(features)
        pred = child.getNodePrediction
      }
      while (!child.isLeaf || !child.isEmptyNode)
      pred
    }
  }
  
  def isEmptyNode = false
  
  def isLeaf = leftChild.isEmptyNode && rightChild.isEmptyNode
  
  def insertChildren(leftChild: Node,
      rightChild: Node,
      values: ArrayBuffer[FeatureValue]): Unit = {}
  
  def replaceNode(newNode: Node): Unit = {}
  
  // **************************************************************************
  //
  // Protected
  //
  // **************************************************************************
  
  protected val id: Int = -1
  protected val featureIndex: Int = -1
  @BeanProperty protected var leftChild: Node = EmptyNode.getEmptyNode
  @BeanProperty protected var rightChild: Node = EmptyNode.getEmptyNode
  protected val prediction: Double = 0.0
}

// A node for ordered data, e.g. value <= 1 and value > 1
// id: The id for the node
// featureIndex: The index into the array of FeatureValue that this splits.
// prediction: The prediction value, provided this is a leaf node.
class OrderedNode(override val id: Int,
    override val featureIndex: Int,
    override val prediction: Double) extends Node {
  var splitValue: FeatureValue = new FeatureValue(0)

  // Return the child leaf node corresponding to the array of values.
  // All features are assumed present. If not enough features are provided
  // then return an EmptyNode.
  override def getLeaf(features: Array[FeatureValue]): Node = {
    if (isLeaf) this
    else if (features.length <= featureIndex) EmptyNode.getEmptyNode
    else if (features(featureIndex) < splitValue) leftChild
    else rightChild
  }

  // Insert both left and right children and set the splitting value.
  override def insertChildren(newLeftChild: Node,
      newRightChild: Node,
      values: ArrayBuffer[FeatureValue]): Unit = {
    leftChild = newLeftChild
    rightChild = newRightChild
    splitValue = values(0)
  }
}

// A node for categorical data, e.g. for values in (1, 2, 3, 4)
// id: The id for the node
// featureIndex: The index into the array of FeatureValue that this splits.
// prediction: The prediction value, provided this is a leaf node.
class CategoricalNode(override val id: Int,
    override val featureIndex: Int,
    override val prediction: Double) extends Node {
  var categories: Set[FeatureValue] = null
  
  // Return the child leaf node corresponding to the array of values.
  // All features are assumed present. If not enough features are provided
  // then return an EmptyNode.
  override def getLeaf(features: Array[FeatureValue]): Node = {
    if (isLeaf) this
    else if (features.length <= featureIndex) EmptyNode.getEmptyNode
    else if (categories.contains(features(featureIndex))) leftChild
    else rightChild
  }
  
  // Insert both left and right children and set the splitting values.
  override def insertChildren(newLeftChild: Node,
      newRightChild: Node,
      values: ArrayBuffer[FeatureValue]): Unit = {
    leftChild = newLeftChild
    rightChild = newRightChild
    categories = values.toSet[FeatureValue]  
  }
}

// A type of data node used for a leaf when growing a tree because
// the feature index has not yet been decided.
class UnstructuredNode(
    override val prediction: Double,
    val parent: Node) extends Node {
  
  override def replaceNode(newNode: Node): Unit = {
    if (this eq parent.getLeftChild) {
      parent.setLeftChild(newNode)
    } else if (this eq parent.getRightChild) {
      parent.setRightChild(newNode)
    }
  }
}