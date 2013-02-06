package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

abstract class PointIterator {
  def hasNext(): Boolean
  def next(): Point
  def reset(): Unit
}

class TreeGrower(val tree: Tree,
    val numFeatures: Int,
    val featureTypes: ArrayBuffer[FeatureType],
    val pointIterator: PointIterator) {
  
  def findNodesToInvestigate(): Map[Int, NodeSplit] = {
    val leaves: ArrayBuffer[Node] = tree.getLeaves
    val nodesToInvestigate = new HashMap[Int, NodeSplit]
    for (val node <- leaves) {
      if (!bestSplitForNode.contains(node)) 
        nodesToInvestigate.put(node.getId, new NodeSplit)
    }
    nodesToInvestigate
  }

  def createNodeSplits(nodeSplits: ArrayBuffer[NodeSplit]): Unit = {
    for (index <- 0 until featureTypes.size) {
      val featureType = featureTypes(index)
      if (featureType.isOrdered) {
        nodeSplits.append(new OrderedNodeSplit(index))
      } else if (featureType.isCategorical) {
        nodeSplits.append(new CategoricalNodeSplit(index))
      }
    }
  }
  
  def Grow(): Unit = {
    while (!tree.isFull) {
      val nodesToInvestigate = findNodesToInvestigate()
      if (!nodesToInvestigate.isEmpty) {
        // For now training is in-memory
    	pointIterator.reset()
        while (pointIterator.hasNext()) {
          val point = pointIterator.next()
          val leaf = tree.getLeaf(point.features)
          // TODO: check for null leaf
          if (nodesToInvestigate.contains(leaf.getId)) {
            val nodeSplits = nodesToInvestigate(leaf.getId)
            if (nodeSplits.isEmpty) createNodeSplits(nodeSplits)
            nodeSplits.foreach(nodeSplit => nodeSplit.process(point))
          }
        }
    	while (!nodesToInvestigate.isEmpty) {
    	  val nodeSplit: NodeSplit = nodesToInvestigate.remove(nodesToInvestigate.size - 1).get
    	}
      }
    }
  }
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  // A map from a Node to the Best Split for the node. Each element of
  // the array is a split based on a different feature.
  private val bestSplitForNode = new HashMap[Node, BestSplit]
}

class Tree(var weight: Double, val maxNodes: Int) {    
  // Return all of the leaves for the tree. In the event of an empty tree,
  // the head node is returned, which is an instance of EmptyNode.
  def getLeaves: ArrayBuffer[Node] = {
    val leaves = new ArrayBuffer[Node]
    def findLeaves(current: Node): Unit = {
      if (current.isLeaf) leaves.append(current)
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
  
  def getLeaf(features: Array[FeatureValue]): Node = head.getLeaf(features)
    
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