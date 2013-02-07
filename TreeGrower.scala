package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

// A class for growing the number of nodes in a tree.
//
// tree:          The tree to grow.
// featureTypes:  An array of FeatureTypes (ordered or categorical),
//                in the same order as a feature vector.
// pointIterator: An iterator over the training data.
class TreeGrower(val tree: Tree,
    val featureTypes: Array[FeatureType],
    val pointIterator: PointIterator) {
  
  // Grow the tree until it has a sufficient number of nodes.
  def Grow(): Unit = {
    val bestSplitForNode = new HashMap[Node, BestSplit]
    while (!tree.isFull) {
      val nodesToInvestigate = findNodesToInvestigate()
      if (!nodesToInvestigate.isEmpty) {
        // For now training is in-memory
    	pointIterator.reset()
        while (pointIterator.hasNext()) {
          val point = pointIterator.next()
          val leaf = tree.getLeaf(point.features)
          // TODO: check for null leaf and apply logic here.
          if (nodesToInvestigate.contains(leaf)) {
            val nodeSplit = nodesToInvestigate(leaf)
            nodeSplit.process(point)
          }
        }
    	while (!nodesToInvestigate.isEmpty) {
    	  val (node, nodeSplit) = nodesToInvestigate.first
    	  bestSplitForNode.put(node, nodeSplit.findBestSplit())
    	  // Free memory for the next split calculation
    	  nodesToInvestigate.remove(node)
    	}
      }
      val (node, bestSplit) = bestSplitForNode.minBy(_._2)
      growTreeAtNode(node, bestSplit)
      bestSplitForNode.remove(node)
    }
  }
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  // Find nodes for which we have not yet determined a best split.
  private def findNodesToInvestigate(): Map[Node, NodeSplit] = {
    val leaves: ArrayBuffer[Node] = tree.getLeaves
    val nodesToInvestigate = new HashMap[Node, NodeSplit]
    for (val node <- leaves) {
      if (!bestSplitForNode.contains(node))
        nodesToInvestigate.put(node,
            new NodeSplit(node.getFeatureIndex, featureTypes))
    }
    nodesToInvestigate
  }
  
  // Return a function for creating Nodes of the appropriate type.
  private def createNode(node: Node): (Int, Double) => Node = {
    def createNodeHelper(node: Node)(featureIndex: Int, prediction: Double) = {
      if (featureTypes(node.getFeatureIndex).isOrdered) {
        nextId += 1
        new OrderedNode(nextId, featureIndex, prediction)
      } else {
        nextId += 1
        new CategoricalNode(nextId, featureIndex, prediction)
      }
    }
    createNodeHelper(node)
  }
  
  // Increase the size of the tree by two, by growing a left and
  // right child.
  //
  // node:      The leaf node at which to grow children.
  // bestSplit: Data about how to split the node.
  private def growTreeAtNode(node: Node, bestSplit: BestSplit): Unit = {
    val nodeCreator = createNode(node)
    val leftChild: Node =
      nodeCreator(bestSplit.featureIndex, bestSplit.leftPrediction)
    val rightChild: Node =
      nodeCreator(bestSplit.featureIndex, bestSplit.leftPrediction)      
    node.insertChildren(leftChild, rightChild, bestSplit.splitFeatures)
  }
  
  // A map from a Node to the Best Split for the node. Each element of
  // the array is a split based on a different feature.
  private val bestSplitForNode = new HashMap[Node, BestSplit]
  
  // The next id to use for creating a new node.
  private var nextId = 1
}