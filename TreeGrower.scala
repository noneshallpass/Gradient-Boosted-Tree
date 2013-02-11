package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.reflect.BeanProperty

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
          if (nodesToInvestigate.contains(leaf)) {
            val nodeSplit = nodesToInvestigate(leaf)
            nodeSplit.process(point)
          }
        }
    	while (!nodesToInvestigate.isEmpty) {
    	  val (node, nodeSplit) = nodesToInvestigate.first
    	  bestSplitForNode.put(node, nodeSplit.findBestSplit())
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
  
  // Find nodes for which we have not yet determined a best split. If the tree
  // is empty, a map with an EmptyNode as a key is returned.
  private def findNodesToInvestigate(): Map[Node, NodeSplit] = {
    val leaves: ArrayBuffer[Node] = tree.getLeaves
    if (leaves.isEmpty) leaves.append(EmptyNode.getEmptyNode)
    val nodesToInvestigate = new HashMap[Node, NodeSplit]
    for (val node <- leaves) {
      if (!bestSplitForNode.contains(node))
        nodesToInvestigate.put(node, new NodeSplit(featureTypes))
    }
    nodesToInvestigate
  }
  
  // All leaves are UnstructuredNodes. If and when, we grow the tree at
  // a leaf, we need to change the node to an ordered or categorical node
  // since we now know what feature we are using to split. If node is
  // an instance of EmptyNode, the tree is empty so we grow the tree at the
  // root.
  //
  // node: The node to replace. By construction, it will be an Unstructured
  //       Node.
  // bestSplit: The data for splitting the node.
  private def replaceNode(node: Node, bestSplit: BestSplit): Node = {
    var newNode: Node = null
    if (featureTypes(bestSplit.featureIndex).isOrdered) {
      newNode = new OrderedNode(nextId,
          bestSplit.featureIndex,
          node.getNodePrediction)
    } else {
      newNode = new CategoricalNode(nextId,
          bestSplit.featureIndex,
          node.getNodePrediction)
    }
    nextId += 1
    if (node.isEmptyNode) tree.setRootNode(newNode)
    else node.replaceNode(newNode)
    newNode
  } 
  
  // Increase the size of the tree by two, by growing a left and
  // right child.
  //
  // node:      The leaf node at which to grow children.
  // bestSplit: Data about how to split the node.
  private def growTreeAtNode(node: Node, bestSplit: BestSplit): Unit = {
    val parent = replaceNode(node, bestSplit)
    val leftChild = new UnstructuredNode(bestSplit.leftPrediction, parent)
    val rightChild = new UnstructuredNode(bestSplit.rightPrediction, parent)
    tree.insertChildren(parent, leftChild, rightChild, bestSplit.splitFeatures)
  }
  
  // A map from a Node to the Best Split for the node. Each element of
  // the array is a split based on a different feature.
  private val bestSplitForNode = new HashMap[Node, BestSplit]
  
  // The next id to use for creating a new node.
  private var nextId = 1
}