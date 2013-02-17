package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer

// A forest is a collection of trees used in prediction.
// TODO: Finish this class.
class Forest {
  
  // Return the prediction for a group of features.
  def getPrediction(features : Array[FeatureValue]): Double = {
    trees.map(tree => tree.getPrediction(features)).sum
  }

  // **************************************************************************
  //
  // Functions for growing the forest.
  //
  // **************************************************************************
  
  def addTree(tree: Tree): Unit = trees.append(tree)
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private val trees = new ArrayBuffer[Tree];
}