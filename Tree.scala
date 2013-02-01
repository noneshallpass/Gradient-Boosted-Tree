package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer;

class Tree {
  
  def getPrediction(features : FeatureList): Double = {
    trees.map(x => x.getPrediction(features)).sum
  }
  
  private class TreeNode(val node: Node, var weight: Double) {
    def getPrediction(features: FeatureList): Double = {
      weight * node.getPrediction(features)
    }
  }
  
  private val trees = new ArrayBuffer[TreeNode];
}