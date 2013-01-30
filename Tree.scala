package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Set

class BestSplit(val splitFeatures: Set[FeatureValue],
    val leftPrediction: Double,
    val rightPrediction: Double) {
}

abstract class NodeSplit {
  def process(point: Point): Unit
  def findBestSplit(): BestSplit
}

// A class to store the (count, y-value sum) for each matching feature value.
class FeatureValueData(var featureCount: Int, var yValSum: Double) {
  def +(that: FeatureValueData): FeatureValueData = {
    new FeatureValueData(featureCount + that.featureCount,
        yValSum + that.yValSum)
  }
  // Accumulate another y-value
  def accumulate(thatYVal: Double): Unit = {
    featureCount += 1
    yValSum += thatYVal
  }
}
private object FeatureValueData {
  def defaultValue = new FeatureValueData(0, 0)
}

// Calculate the best split for a categorical node.
// Ctor arguments:
//    featureIndex: the index of the FeatureValue within a Point to
//                  evaluate.
// Use this like:
//
// for (point <- allPoints) categoricalNodeSplit.process(point)
// bestSplit = categoricalNodeSplit.findBestSplit()
class CategoricalNodeSplit(val featureIndex: Int) extends NodeSplit {
  // Accumulate statistics regarding a point of data.
  def process(point: Point): Unit = {
    val featureValue = point.featureValues(featureIndex)
    data(featureValue).accumulate(point.yValue)
  }
  
  def findBestSplit(): BestSplit = {
    // Convert to a sequence of (FeatureValue, (Count, y-value sum))
    // and sort by the average y-value.
    val sortedData = data.toSeq.sortBy(
        { case (_, featureValueData) =>
          featureValueData.yValSum / featureValueData.featureCount })
    data.clear()
    
    // The error is Sum (y_i - prediction)^2 over region R_i
    // The solution is prediction = average y_i over region R_i, with 
    // error = Sum y_i ^2 - 2 * prediction * Sum y_i + N * prediction^2
    // where N is the number of points in region R_i. To determine the best
    // region split, we accumulate the individual components of sortedData.
    def accumulate(x: FeatureValueAndData, y: FeatureValueAndData) =
      (y._1, x._2 + y._2)
    // The first element of accumulatedData is garbage. The real index starts
    // at 1.
    val accumulatedData = sortedData.toSeq.scanLeft(
        (new FeatureValue(0), FeatureValueData.defaultValue))(accumulate)
    val predictionsAndRelError= for (index <- 1 until accumulatedData.size)
      yield computePredictionsAndRelativeError(index, accumulatedData)
    val bestPredictionAndError = predictionsAndRelError.minBy(_.relativeError)
    // The 0 element is garbage.
    val splitData = accumulatedData.dropRight(
        accumulatedData.size - bestPredictionAndError.index - 1)
    val splitFeatures = new HashSet[FeatureValue]
    for (index <- 1 until splitData.size) {
      splitFeatures += splitData(index)._1
    }
    new BestSplit(splitFeatures,
    		bestPredictionAndError.leftPrediction,
    		bestPredictionAndError.rightPrediction)
  }
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  // A map from a FeatureValue/x-value (Double, Int, or String) to data 
  // representing (count, y-value sum). We will use Breiman's algorithm
  // for determining an optimal split by examining splits ordered by increasing
  // average y-value.
  private val data = new HashMap[FeatureValue, FeatureValueData].
    withDefaultValue(FeatureValueData.defaultValue)
   
  private type FeatureValueAndData = (FeatureValue, FeatureValueData)
  
  // A helper class to contain the prediction index, and the prediction
  // values over the left and right regions along with the relative
  // prediction error.
  private class PredictionAndRelativeError(
      val index: Int,
      val leftPrediction: Double,
      val rightPrediction: Double,
      val relativeError: Double) {
  }
  
  // Compute the predictions for the left and right regions and the
  // relative prediction error based on a data split at an index.
  private def computePredictionsAndRelativeError(
      index: Int,
      accumulatedData: Seq[FeatureValueAndData]):
      PredictionAndRelativeError = {
    val first = accumulatedData(index)
    val last = accumulatedData.last
    // The average y-value over the left partition.
    val leftPrediction = first._2.yValSum / first._2.featureCount
    // The average y-value over the right partition.
    val rightPrediction = (last._2.yValSum - first._2.yValSum) /
    	(last._2.featureCount - first._2.featureCount)
    // The Sum of y_i ^2 is constant amongst all errors calculations so it
    // can be ignored. Define a function to calculate the 
    def relativePartitionError(
        prediction: Double,
        first: FeatureValueData,
        last: FeatureValueData) = {
      - 2 * prediction * (last.yValSum - first.yValSum) +
      (last.featureCount - first.featureCount) * prediction * prediction
    }
    // Sum the relative errors of the left and right partitions.
    val relativeError =
      relativePartitionError(leftPrediction,
          FeatureValueData.defaultValue,
          first._2) +
      relativePartitionError(rightPrediction, first._2, last._2)
    new PredictionAndRelativeError(index,
        leftPrediction,
        rightPrediction,
        relativeError)
  }
}

class OrderedNodeSplit(val featureIndex: Int) extends NodeSplit {
  def process(point: Point): Unit = {
    data += new FeatureData(point.featureValues(featureIndex),
        point.yValue)
  }
  
  def findBestSplit(): BestSplit = {
    // TODO: find an in-place sort
    val sortedData = data.sortBy(_.featureValue)
    data.clear()
    
    
    // The error is Sum (y_i - prediction)^2 over region R_i
    // The solution is prediction = average y_i over region R_i, with 
    // error = Sum y_i ^2 - 2 * prediction * Sum y_i + N * prediction^2
    // where N is the number of points in region R_i. To determine the best
    // region split, we accumulate the individual components of sortedData.
    def accumulate(x: FeatureValueAndData, y: FeatureValueAndData) =
      (y._1, x._2 + y._2)
    // The first element of accumulatedData is garbage. The real index starts
    // at 1.
    val accumulatedData = sortedData.toSeq.scanLeft(
        (new FeatureValue(0), FeatureValueData.defaultValue))(accumulate)
    val predictionsAndRelError= for (index <- 1 until accumulatedData.size)
      yield computePredictionsAndRelativeError(index, accumulatedData)
    val bestPredictionAndError = predictionsAndRelError.minBy(_.relativeError)
    // The 0 element is garbage.
    val splitData = accumulatedData.dropRight(
        accumulatedData.size - bestPredictionAndError.index - 1)
    val splitFeatures = new HashSet[FeatureValue]
      splitFeatures += splitData(index)._1
    // TODO: fix temporary return value.
    new BestSplit(splitFeatures, bestPredictionAndError, 0)
  }
  
  
  private val data = new ArrayBuffer[FeatureData]
  private class FeatureData(val featureValue: FeatureValue, val yValue: Double) {
  }
}


class TreeGrower(val numNodes: Int) {
  var tree: Node = new EmptyNode
  def Grow(point: Point) {
    
    
  }
}