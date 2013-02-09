package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

// A classification or regression tree. The tree is trained by
// the class TreeGrower.
//
// weight:   The weight to associate with this tree in prediction.
// maxNodes: The maximum number of nodes for this tree.
class Tree(var weight: Double, val maxNodes: Int) {
  
  // Return all of the leaves for the tree. In the event of an empty tree,
  // the root node is returned, which is an instance of EmptyNode.
  def getLeaves: ArrayBuffer[Node] = {
    val leaves = new ArrayBuffer[Node]
    def findLeaves(current: Node): Unit = {
      if (current.isLeaf) leaves.append(current)
      else {
        findLeaves(current.getLeftChild)
        findLeaves(current.getRightChild)
      }
    }
    if (leaves.isEmpty) leaves.append(root)
    findLeaves(root)
    leaves
  }
  
  // Return the weighted prediction for this tree.
  def getPrediction(features: Array[FeatureValue]): Double = {
    weight * root.getPrediction(features)
  }
  
  // Return the leaf node corresponding to a vector of features.
  def getLeaf(features: Array[FeatureValue]): Node = root.getLeaf(features)
    
  // Indicates whether the tree is fully grown or not.
  def isFull: Boolean = nodeCount < maxNodes
  
  def setRootNode(node: Node): Unit = root = node
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  // The number of nodes in the tree.
  private val nodeCount: Int = 0
  
  // The root node for the tree.
  private var root: Node = EmptyNode.getEmptyNode
}