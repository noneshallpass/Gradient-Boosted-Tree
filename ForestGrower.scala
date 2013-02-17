package gradientBoostedTree

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
    // TODO: Create initial tree.
    for (i <- 1 until maxTrees) {
      val newTree = new Tree(1.0, maxNodesPerTree)
      // TODO: Make the loss function selectable.
      val lossFunction = new SquaredLoss
      val differentialIterator =
        new DifferentialPointIterator(pointIterator, lossFunction, forest)
      val treeGrower =
        new TreeGrower(newTree, featureTypes, differentialIterator)
      // TODO: Implement step to re-calculate predictions for new tree based on
      // loss function minimization at each leaf.
      forest.addTree(newTree)
    }
    forest
  }
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
}