package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer

// A forest is a collection of trees used in prediction.
// TODO: Finish this class.
class Forest {
  
  // Return the prediction for a group of features.
  def getPrediction(features : Array[FeatureValue]): Double = {
    trees.map(x => x.getPrediction(features)).sum
  }

  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private val trees = new ArrayBuffer[Tree];
}