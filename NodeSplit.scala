package gradientBoostedTree

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Set

// The best splitting of either an ordered or categorical node,
// including the predictions for the left and right branches.
class BestSplit(
    val featureIndex: Int,
    val splitFeatures: ArrayBuffer[FeatureValue],
    val leftPrediction: Double,
    val rightPrediction: Double,
    val leftError: Double,
    val rightError: Double,
    val leftCount: Int,
    val rightCount: Int) extends Ordered[BestSplit] {
  override def compare(other: BestSplit): Int = error.compare(other.error)
  
  def error: Double = leftError + rightError
  
  def isNotASolution: Boolean = {
    leftError + rightError == Double.PositiveInfinity
  }
  
  override def toString(): String = "(%d, %s, %f, %f, %f, %f, %d, %d)".format(
      featureIndex, splitFeatures.toString(), leftPrediction, rightPrediction,
      leftError, rightError, leftCount, rightCount)
}

object BestSplit {
	def noSolution: BestSplit = new BestSplit(-1, ArrayBuffer(),
	    0.0, 0.0, Double.PositiveInfinity, Double.PositiveInfinity, 0, 0)
}


// A class used to find the best split for a node.
class NodeSplit(val featureTypes: Array[FeatureType]) {
  // Accumulate statistics regarding a point of data.
  def process(point: Point): Unit = {
    data.append(point)
  }
  
  // Evaluate all splitting features for a node and select the one with
  // the smallest error.
  def findBestSplit(): BestSplit = {
    var bestSplit: BestSplit = null
    for (featureIndex <- 0 until featureTypes.length) {
      val featureType = featureTypes(featureIndex)
      var currentBestSplit: BestSplit = null
      if (featureType.isOrdered) {
        currentBestSplit = orderedSplit.findBestSplit(featureIndex, data)
      } else currentBestSplit = categoricalSplit.findBestSplit(featureIndex, data)
      if (bestSplit == null || currentBestSplit.error < bestSplit.error) {
        bestSplit = currentBestSplit
      }
    }
    bestSplit
  }

  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private val orderedSplit: NodeSplitBase = new OrderedNodeSplit
  private val categoricalSplit: NodeSplitBase = new CategoricalNodeSplit
  private val data = new ArrayBuffer[Point]
}


// A class used to find the best split for a node.
abstract class NodeSplitBase {
  // For a split based on a feature at a given index, find
  // the splitting which minimizes the squared error.
  def findBestSplit(featureIndex: Int, data: ArrayBuffer[Point]): BestSplit
  
  // A helper class to contain the prediction index, and the prediction
  // values over the left and right regions along with the prediction error.
  // This class is a bit redundant with BestSplit above, but consumes slightly
  // less memory.
  protected class PredictionAndError(
      val index: Int,
      val leftPrediction: Double,
      val rightPrediction: Double,
      val leftError: Double,
      val rightError: Double) extends Ordered[PredictionAndError] {
    def error: Double = leftError + rightError
    
    override def compare(other: PredictionAndError) = error.compare(other.error)
    
    override def toString: String = "%d, %f, %f, %f, %f".format(index,
        leftPrediction, rightPrediction, leftError, rightError)
  }
  
  protected object PredictionAndError {
    // Compute the predictions for the left and right regions and the
    // prediction error based on a data split at an index.
    def compute(
        index: Int,
        accumulatedData: ArrayBuffer[FeatureValueAndData]):
        PredictionAndError = {
      val first = accumulatedData(index)
      val last = accumulatedData.last
      // The average y-value over the left partition.
      val leftPrediction = first.yValSum / first.featureCount
      // The average y-value over the right partition.
      var rightPrediction: Double = 0.0
      // accumulatedData.length = #points + 1. To avoid a NaN for the rightPrediction,
      // we must explicitly set it to 0 for a split of 1 point.
      if (accumulatedData.length > 2) {
        rightPrediction = (last.yValSum - first.yValSum) /
          (last.featureCount - first.featureCount)
      }
      def partitionError(
          prediction: Double,
          first: FeatureValueAndData,
          last: FeatureValueAndData) = {
        last.ySqValSum - first.ySqValSum +
        - 2 * prediction * (last.yValSum - first.yValSum) +
        (last.featureCount - first.featureCount) * prediction * prediction
      }
      val leftError =
        partitionError(leftPrediction,
            new FeatureValueAndData(new FeatureValue(0), 0, 0, 0),
            first)
      val rightError = partitionError(rightPrediction, first, last)
      new PredictionAndError(index,
          leftPrediction,
          rightPrediction,
          leftError,
          rightError)
    }
    
    def notASolution: PredictionAndError = new PredictionAndError(0, 0, 0,
        Double.PositiveInfinity, Double.PositiveInfinity)
  }
  
  //
  protected def minError(accumulatedData: ArrayBuffer[FeatureValueAndData],
      predictionAndError: IndexedSeq[PredictionAndError]):
      PredictionAndError = {
    var bestPredictionAndError: PredictionAndError =
      PredictionAndError.notASolution
    // TODO: consider generalizing the node splitting as a protection
    // against noisy data, e.g. probably better not to split
    // the data so that 1 training point goes to the left branch while
    // 1000 training points go to the right.
    for(i <- 0 until predictionAndError.length - 1) {
      if (accumulatedData(i + 1).featureValue !=
        accumulatedData(i + 2).featureValue &&
          predictionAndError(i) < bestPredictionAndError) {
        bestPredictionAndError = predictionAndError(i)
      }
    }
    if (bestPredictionAndError.error ==
      PredictionAndError.notASolution.error) {
      predictionAndError(0)
    } else bestPredictionAndError
  }
  
  // Do not continue with node split calculations if there are too few points.
  protected def hasTooFewPoints(size: Int): Boolean = size <= 1
}

// A class to store the (Feature value, count, y-value sum, y^2-value sum).
class FeatureValueAndData(val featureValue: FeatureValue,
    val featureCount: Int, val yValSum: Double, val ySqValSum: Double) {
  
  // Sum the respective fields.
  def +(that: FeatureValueAndData): FeatureValueAndData = {
    new FeatureValueAndData(that.featureValue,
        featureCount + that.featureCount,
        yValSum + that.yValSum,
        ySqValSum + that.ySqValSum)
  }
  
  def canEqual(other: Any) = other.isInstanceOf[FeatureValueAndData]
  
  override def equals(other: Any) = other match {
    case other: FeatureValueAndData =>
      (other.canEqual(this)) &&
      (featureValue == other.featureValue) &&
      (featureCount == other.featureCount) &&
      (yValSum == other.yValSum)
    case _ => false
  }
  
  override def toString: String = "(%s, %d, %f)".format(
      featureValue.toString(), featureCount, yValSum)
  
  // Return a new object, with the respective data fields summed. The FeatureValue
  // selected is the one corresponding to the parameter. Used for accumulating
  // statistics.
  def accumulate(that: FeatureValueAndData): FeatureValueAndData = {
    new FeatureValueAndData(that.featureValue, featureCount + that.featureCount,
        yValSum + that.yValSum, ySqValSum + that.ySqValSum)
  }
  
  // Return a function for accumulate statistics by incorporating an additional
  // point using the data for a particular feature index.
  def accumulate(featureIndex: Int): (Point) => FeatureValueAndData = {
    def accumulateHelper(featureIndex: Int)(that: Point) = {
      new FeatureValueAndData(that.features(featureIndex), featureCount + 1,
          yValSum + that.yValue, ySqValSum + that.yValue * that.yValue)
    }
    accumulateHelper(featureIndex)
  }
}

// A class to store the (count, y-value sum, y^2-value sum) for each
// matching feature value.
class FeatureValueData(val featureCount: Int,
    val yValSum: Double,
    val ySqValSum: Double) {

  // Add two FeatureValueData objects by summing the respective fields.
  def +(that: FeatureValueData): FeatureValueData = {
    new FeatureValueData(featureCount + that.featureCount,
        yValSum + that.yValSum,
        ySqValSum + that.ySqValSum)
  }

  // Accumulate another y-value.
  def accumulate(thatYVal: Double): FeatureValueData = {
    new FeatureValueData(featureCount + 1, yValSum + thatYVal,
        ySqValSum + thatYVal * thatYVal)
  }
 
  override def toString: String = "(%d, %f)".format(featureCount, yValSum)

  def canEqual(other: Any) = other.isInstanceOf[FeatureValueData]

  override def equals(other: Any) = other match {
    case other: FeatureValueData =>
      (other.canEqual(this)) &&
      (featureCount == other.featureCount) &&
      (yValSum == other.yValSum)
    case _ => false
  }
}

// Find the best split for a categorical feature.
class CategoricalNodeSplit extends NodeSplitBase {
  
  override def findBestSplit(featureIndex: Int,
      data: ArrayBuffer[Point]): BestSplit = {
    if (hasTooFewPoints(data.size)) return BestSplit.noSolution
    // A map from a FeatureValue/x-value (Double, Int, or String) to data 
    // representing (count, y-value sum). We will use Breiman's algorithm
    // for determining an optimal split by examining splits ordered by increasing
    // average y-value.
    val featureAndDataMap = new HashMap[FeatureValue, FeatureValueData].
      withDefaultValue(new FeatureValueData(0, 0, 0))
    for (point <- data) {
      val featureValue = point.features(featureIndex)
      featureAndDataMap.put(featureValue,
          featureAndDataMap(featureValue).accumulate(point.yValue))
    }
    // Convert to an ArrayBuffer of FeatureValueAndData.
    val combinedData = new ArrayBuffer[FeatureValueAndData]
    for ((featureValue, featureValueData) <- featureAndDataMap) { 
      combinedData += new FeatureValueAndData(featureValue,
          featureValueData.featureCount,
          featureValueData.yValSum,
          featureValueData.ySqValSum)
    }
    featureAndDataMap.clear()
    // TODO: replace with an in-place sort
    val sortedData = combinedData.sortBy({ x => x.yValSum / x.featureCount })
    combinedData.clear()
    // The error is Sum (y_i - prediction)^2 over region R_i
    // The solution is prediction = average y_i over region R_i, with 
    // error = Sum y_i ^2 - 2 * prediction * Sum y_i + N * prediction^2
    // where N is the number of points in region R_i. To determine the best
    // region split, we accumulate the individual components of sortedData.
    // The first element of accumulatedData is garbage. The real index starts
    // at 1.
    val accumulatedData = sortedData.scanLeft(new FeatureValueAndData(
        sortedData(0).featureValue, 0, 0, 0))(_ accumulate _)
    sortedData.clear()
    val predictionsAndError= for (index <- 1 until accumulatedData.size)
      yield PredictionAndError.compute(index, accumulatedData)
    val bestPredictionAndError = minError(accumulatedData, predictionsAndError)
    val leftCount = bestPredictionAndError.index
    val rightCount = accumulatedData.size - leftCount - 1
    // The 0 element is garbage.
    val splitData = accumulatedData.dropRight(
        accumulatedData.size - bestPredictionAndError.index - 1)
    val splitFeatures = new ArrayBuffer[FeatureValue]
    for (index <- 1 until splitData.size) {
      splitFeatures += splitData(index).featureValue
    }
    new BestSplit(featureIndex,
        splitFeatures,
        bestPredictionAndError.leftPrediction,
    	bestPredictionAndError.rightPrediction,
    	bestPredictionAndError.leftError,
    	bestPredictionAndError.rightError,
    	leftCount,
    	rightCount)
  }
}

// Find the best split for an ordered feature.
class OrderedNodeSplit extends NodeSplitBase {

  override def findBestSplit(featureIndex: Int,
      data: ArrayBuffer[Point]): BestSplit = {
    if (hasTooFewPoints(data.size)) return BestSplit.noSolution
    // TODO: replace with an in-place sort
    val sortedData = data.sortBy(_.features(featureIndex))

    // The error is Sum (y_i - prediction)^2 over region R_i
    // The solution is prediction = average y_i over region R_i, with 
    // error = Sum y_i ^2 - 2 * prediction * Sum y_i + N * prediction^2
    // where N is the number of points in region R_i. To determine the best
    // region split, we accumulate the individual components of sortedData.
    // The first element of accumulatedData is garbage. The real index starts
    // at 1.
    val accumulatedData = sortedData.scanLeft(
        new FeatureValueAndData(
            sortedData(0).features(featureIndex), 0, 0, 0))(
                _.accumulate(featureIndex)(_))
    sortedData.clear()
    val predictionsAndError = for (index <- 1 until accumulatedData.size)
      yield PredictionAndError.compute(index, accumulatedData)
    val bestPredictionAndError = minError(accumulatedData, predictionsAndError)
    val leftCount = bestPredictionAndError.index
    val rightCount = accumulatedData.size - leftCount - 1
    // The 0 element is garbage.
    val splitData = accumulatedData.dropRight(
        accumulatedData.size - bestPredictionAndError.index - 1)
    val splitFeatures = new ArrayBuffer[FeatureValue]
    splitFeatures += accumulatedData(bestPredictionAndError.index).featureValue
    new BestSplit(featureIndex,
        splitFeatures,
        bestPredictionAndError.leftPrediction,
        bestPredictionAndError.rightPrediction,
        bestPredictionAndError.leftError,
        bestPredictionAndError.rightError,
        leftCount,
        rightCount)
  }

  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private class FeatureData(val featureValue: FeatureValue, val yValue: Double) {
  }
}