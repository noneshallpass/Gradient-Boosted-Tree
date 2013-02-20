package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer
import scala.reflect.BeanProperty

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
  def this(pred: Double) {
    this()
    prediction = pred
  }
  
  // Node errors are currently only stored at the leaves.
  def getError: Double = Double.PositiveInfinity
  
  // Get the node id.
  def getId = id
  
  // Get the index of the feature that this node splits.
  def getFeatureIndex = featureIndex
  
  // Get the leaf node corresponding to an array of feature values.
  def getLeaf(features: Array[FeatureValue]): Node = EmptyNode.getEmptyNode
  
  // Get the prediction associated with this node, which may or may not be a
  // leaf node.
  def getNodePrediction: Double = prediction
  
  // Given the array of feature values, find the prediction for the corresponding
  // leaf node.
  def getPrediction(features : Array[FeatureValue]): Double = {
    if (isEmptyNode) 0.0
    else {
      val leaf = getLeaf(features)
      if (leaf.isEmptyNode) 0.0
      else leaf.getNodePrediction
    }
  }
  
  def isEmptyNode = false
  
  def isLeaf = leftChild.isEmptyNode && rightChild.isEmptyNode
  
  // Insert a left and right child at a node, which must be a leaf.
  // values: The splitting values. For an ordered node, this must
  //         have exactly one.
  def insertChildren(newLeftChild: Node,
      newRightChild: Node,
      values: ArrayBuffer[FeatureValue]): Unit = {
    leftChild = newLeftChild
    rightChild = newRightChild
  }
  
  // Replace this node with a new one. Must be used on leaves only.
  def replaceNode(newNode: Node): Unit = {}
  
  private[gradientBoostedTree] def setPrediction(
      pred: Double): Unit = prediction = pred
  
  // Generate a string of the leaves with the prediction and splitting value(s)
  override def toString: String = {
    stringTraverse("")
  }

  // Return the splitting value as a string.
  def splitString: String = ""
  
  // **************************************************************************
  //
  // Protected
  //
  // **************************************************************************
  
  protected val id: Int = -1
  protected val featureIndex: Int = -1
  @BeanProperty protected var leftChild: Node = EmptyNode.getEmptyNode
  @BeanProperty protected var rightChild: Node = EmptyNode.getEmptyNode
  protected var prediction: Double = 0.0
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  // A helper function used by toString.
  private def stringTraverse(path: String): String = {
      if (!leftChild.isEmptyNode && !rightChild.isEmptyNode) {
        "%s :: %s".format(leftChild.stringTraverse(path + "L"),
            rightChild.stringTraverse(path + "R"))     
      } else {
        "%s = %f @ %s".format(path, prediction, splitString)
      }
  }
}

// A node for ordered data, e.g. value <= 1 and value > 1
// id: The id for the node
// featureIndex: The index into the array of FeatureValue that this splits.
// prediction: The prediction value, provided this is a leaf node.
class OrderedNode(override val id: Int,
    override val featureIndex: Int,
    prediction: Double) extends Node(prediction) {

  // Return the child leaf node corresponding to the array of values.
  // All features are assumed present. If not enough features are provided
  // then return an EmptyNode.
  override def getLeaf(features: Array[FeatureValue]): Node = {
    if (isLeaf) this
    else if (features.length <= featureIndex) EmptyNode.getEmptyNode
    else if (features(featureIndex) <= splitValue) leftChild.getLeaf(features)
    else rightChild.getLeaf(features)
  }

  // Insert both left and right children and set the splitting value.
  override def insertChildren(newLeftChild: Node,
      newRightChild: Node,
      values: ArrayBuffer[FeatureValue]): Unit = {
    leftChild = newLeftChild
    rightChild = newRightChild
    splitValue = values(0)
  }

  override def splitString: String = splitValue.toString
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private var splitValue: FeatureValue = new FeatureValue(0)
}

// A node for categorical data, e.g. for values in (1, 2, 3, 4)
// id: The id for the node
// featureIndex: The index into the array of FeatureValue that this splits.
// prediction: The prediction value, provided this is a leaf node.
class CategoricalNode(override val id: Int,
    override val featureIndex: Int,
    prediction: Double) extends Node(prediction) {
  
  // Return the child leaf node corresponding to the array of values.
  // All features are assumed present. If not enough features are provided
  // then return an EmptyNode.
  override def getLeaf(features: Array[FeatureValue]): Node = {
    if (isLeaf) this
    else if (features.length <= featureIndex) EmptyNode.getEmptyNode
    else if (categories.contains(features(featureIndex))) {
      leftChild.getLeaf(features)
    } else rightChild.getLeaf(features)
  }
  
  // Insert both left and right children and set the splitting values.
  override def insertChildren(newLeftChild: Node,
      newRightChild: Node,
      values: ArrayBuffer[FeatureValue]): Unit = {
    leftChild = newLeftChild
    rightChild = newRightChild
    categories = values.toSet[FeatureValue]  
  }
  
  override def splitString: String = categories.toString
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private var categories: Set[FeatureValue] = null
}

// A type of data node used for a leaf when growing a tree because
// the feature index has not yet been decided.
class UnstructuredNode(
    prediction: Double,
    val parent: Node,
    val error: Double) extends Node(prediction) {
  
  override def getError: Double = error
  
  // Unstructured nodes can only be leaves.
  override def getLeaf(features: Array[FeatureValue]): Node = this
  
  // Replace the Unstructured node with another node, which will either
  // be an ordered or categorical node. Assumes that parent is
  // not an EmptyNode.
  override def replaceNode(newNode: Node): Unit = {
    if (this eq parent.getLeftChild) {
      parent.setLeftChild(newNode)
    } else if (this eq parent.getRightChild) {
      parent.setRightChild(newNode)
    }
  }
  
  override def splitString: String = parent.splitString
}