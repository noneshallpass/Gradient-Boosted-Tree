package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.HashMap

abstract class PointIterator {
  def hasNext(): Boolean
  def Next(): Point
  def Reset(): Unit
}

class TreeGrower(val tree: Tree,
    val numFeatures: Int,
    val featureTypes: ArrayBuffer[FeatureType],
    val pointIterator: PointIterator) {
  
  def updateWithNewSplits: Unit = {
    val leaves: ArrayBuffer[Node] = tree.getLeaves
    leaves.foreach(node =>
      if (!splits.contains(node)) splits.put(node, new ArrayBuffer[NodeSplit]))
  }

  def Grow() {
    while (!tree.isFull) {
      pointIterator.Reset()
      while (pointIterator.hasNext()) {
        val point = pointIterator.Next()
        val child = tree.getChild(point.features)
      }
    }
  }
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private val splits = new HashMap[Node, ArrayBuffer[NodeSplit]]
}

class Tree(var weight: Double, val maxNodes: Int) {    
  // Return all of the leaves for the tree. In the event of an empty tree,
  // the head node is returned, which is an instance of EmptyNode.
  def getLeaves: ArrayBuffer[Node] = {
    val leaves = new ArrayBuffer[Node]
    def findLeaves(current: Node): Unit = {
      if (current.isLeaf) leaves += current
      else {
        findLeaves(current.getLeftChild)
        findLeaves(current.getRightChild)
      }
    }
    if (leaves.isEmpty) leaves.append(head)
    findLeaves(head)
    leaves
  }
    
  def getPrediction(features: Array[FeatureValue]): Double = {
    weight * head.getPrediction(features)
  }
  
  def getChild(features: Array[FeatureValue]): Node = head.getChild(features)
    
  def isFull: Boolean = nodeCount < maxNodes
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private val nodeCount: Int = 0
  private var head: Node = new EmptyNode
}

class Forest(val numNodes: Int) {
  
  def getPrediction(features : Array[FeatureValue]): Double = {
    trees.map(x => x.getPrediction(features)).sum
  }
  
  private class Tree(val node: Node, var weight: Double) {
    private val nodeCount: Int = 0
    
    // Return all of the leaves for the tree
    def getLeaves: ArrayBuffer[Node] = {
      val leaves = new ArrayBuffer[Node]
      def findLeaves(current: Node): Unit = {
        if (current.isLeaf) leaves += current
        else {
          findLeaves(current.getLeftChild)
          findLeaves(current.getRightChild)
        }
      }
      findLeaves(node)
      leaves
    }
    
    def getPrediction(features: Array[FeatureValue]): Double = {
      weight * node.getPrediction(features)
    }
    
    def isFull = nodeCount < numNodes
  }
  
  private val trees = new ArrayBuffer[Tree];
}