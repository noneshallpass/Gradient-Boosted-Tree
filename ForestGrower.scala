package gradientBoostedTree

import scala.collection.mutable.HashMap

// A class for growing a forest of trees based on gradient boosting.
//
// maxTrees:        The number of trees to train.
// maxNodesPerTree: The maximum number of nodes per tree.
// featureTypes:    An array of the types of each feature, e.g. an array
//                  indicating whether the feature at a particular index
//                  is either an ordered or categorical feature.
// pointIterator:   An iterator over the training data.
//
// TODO: finish class
class ForestGrower(val maxTrees: Int,
    val maxNodesPerTree: Int,
    val featureTypes: Array[FeatureType],
    val pointIterator: PointIterator) {

  def Grow(): Forest = {
    val forest: Forest = new Forest
    createInitialTree(featureTypes, pointIterator, forest)
    for (i <- 1 until maxTrees) {
      val newTree = new Tree(1.0, maxNodesPerTree)
      // TODO: Make the loss function selectable.
      val lossFunction = new SquaredLoss
      val differentialIterator =
        new DifferentialPointIterator(pointIterator, lossFunction, forest)
      val treeGrower =
        new TreeGrower(newTree, featureTypes, differentialIterator)
      reTrainLeaves(newTree, lossFunction)
      forest.addTree(newTree)
    }
    forest
  }
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private def createInitialTree(featureTypes: Array[FeatureType],
      pointIterator: PointIterator, forest: Forest): Unit = {
    // A single node tree will train to the average y-value
    val newTree = new Tree(1.0, 1)
    val treeGrower =
        new TreeGrower(newTree, featureTypes, pointIterator)
    treeGrower.Grow()
    forest.addTree(newTree)
  }
  
  private def reTrainLeaves(newTree: Tree,
      lossFunction: LossFunction): Unit = {
    val leaves = newTree.getLeaves
    val nodeToRetrainerMap = new HashMap[Node, LeafRetrainer]
    pointIterator.reset()
    while (pointIterator.hasNext()) {
      val point = pointIterator.next()
      val leaf = newTree.getLeaf(point.features)
      if (!nodeToRetrainerMap.contains(leaf)) {
        nodeToRetrainerMap.put(leaf,
            LeafRetrainer.createRetrainer(lossFunction))
      }
      val retrainer: LeafRetrainer = nodeToRetrainerMap.get(leaf).get
      retrainer.process(point)
    }
    for ((leaf, retrainer) <- nodeToRetrainerMap) {
      val newPrediction: Double = retrainer.getPrediction()
      leaf.setPrediction(newPrediction)
    }
  }
}