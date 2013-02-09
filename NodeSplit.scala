package gradientBoostedTree

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
    val error: Double) extends Ordered[BestSplit] {
  override def compare(other: BestSplit): Int = this.compare(other)
}

// A class used to find the best split for a node.
class NodeSplit(val featureIndex: Int, val featureTypes: Array[FeatureType]) {
  val nodeSplit: NodeSplitBase = createNodeSplit()
  
  def createNodeSplit(): NodeSplitBase = {
    val featureType = featureTypes(featureIndex)
	if (featureType.isOrdered) {
	  new OrderedNodeSplit(featureIndex)
    } else {
      new CategoricalNodeSplit(featureIndex)
    }
  }
  
  def process(point: Point): Unit = {
    nodeSplit.process(point)
  }
  
  def findBestSplit(): BestSplit = {
    nodeSplit.findBestSplit()
  }
}


// A class used to find the best split for a node.
abstract class NodeSplitBase {
  def process(point: Point): Unit
  
  def findBestSplit(): BestSplit
  
  // A helper class to contain the prediction index, and the prediction
  // values over the left and right regions along with the relative
  // prediction error.
  protected class PredictionAndRelativeError(
      val index: Int,
      val leftPrediction: Double,
      val rightPrediction: Double,
      val relativeError: Double) {
  }
  
  protected object PredictionAndRelativeError {
    // Compute the predictions for the left and right regions and the
    // relative prediction error based on a data split at an index.
    def compute(
        index: Int,
        accumulatedData: ArrayBuffer[FeatureValueAndData]):
        PredictionAndRelativeError = {
      val first = accumulatedData(index)
      val last = accumulatedData.last
      // The average y-value over the left partition.
      val leftPrediction = first.yValSum / first.featureCount
      // The average y-value over the right partition.
      val rightPrediction = (last.yValSum - first.yValSum) /
        (last.featureCount - first.featureCount)
      // The Sum of y_i ^2 is constant amongst all errors calculations so it
      // can be ignored. Define a function to calculate the 
      def relativePartitionError(
          prediction: Double,
          first: FeatureValueAndData,
          last: FeatureValueAndData) = {
        - 2 * prediction * (last.yValSum - first.yValSum) +
        (last.featureCount - first.featureCount) * prediction * prediction
      }
      // Sum the relative errors of the left and right partitions.
      val relativeError =
        relativePartitionError(leftPrediction,
            new FeatureValueAndData(new FeatureValue(0), 0, 0),
            first) +
      relativePartitionError(rightPrediction, first, last)
      new PredictionAndRelativeError(index,
          leftPrediction,
          rightPrediction,
          relativeError)
    }
  }
}

// A class to store the (Feature value, count, y-value sum).
class FeatureValueAndData(val featureValue: FeatureValue,
    var featureCount: Int, var yValSum: Double) {
  
  def +(that: FeatureValueAndData): FeatureValueAndData = {
    new FeatureValueAndData(that.featureValue,
        featureCount + that.featureCount,
        yValSum + that.yValSum)
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
  
  def accumulate(that: FeatureValueAndData): FeatureValueAndData = {
    new FeatureValueAndData(that.featureValue, featureCount + that.featureCount,
        yValSum + that.yValSum)
  }
  
  def accumulate(featureIndex: Int): (Point) => FeatureValueAndData = {
    def accumulateHelper(featureIndex: Int)(that: Point) = {
      new FeatureValueAndData(that.features(featureIndex), featureCount + 1,
          yValSum + that.yValue)
    }
    accumulateHelper(featureIndex)
  }
}

// A class to store the (count, y-value sum) for each matching feature value.
class FeatureValueData(var featureCount: Int, var yValSum: Double) {

  def +(that: FeatureValueData): FeatureValueData = {
    new FeatureValueData(featureCount + that.featureCount,
        yValSum + that.yValSum)
  }

  // Accumulate another y-value
  def accumulate(thatYVal: Double): FeatureValueData = {
    new FeatureValueData(featureCount + 1, yValSum + thatYVal)
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

// Calculate the best split for a categorical node.
// Ctor arguments:
//    featureIndex: the index of the FeatureValue within a Point to
//                  evaluate.
// Use this like:
//
// for (point <- allPoints) categoricalNodeSplit.process(point)
// bestSplit = categoricalNodeSplit.findBestSplit()
class CategoricalNodeSplit(val featureIndex: Int) extends NodeSplitBase {
  // Accumulate statistics regarding a point of data.
  override def process(point: Point): Unit = {
    //val featureValue = point.features(featureIndex)
    //data(featureValue).accumulate(point.yValue)
    data.append(point)
  }
  
  override def findBestSplit(): BestSplit = {
    // A map from a FeatureValue/x-value (Double, Int, or String) to data 
    // representing (count, y-value sum). We will use Breiman's algorithm
    // for determining an optimal split by examining splits ordered by increasing
    // average y-value.
    val featureAndDataMap = new HashMap[FeatureValue, FeatureValueData].
      withDefaultValue(new FeatureValueData(0, 0))
    for (point <- data) {
      val featureValue = point.features(featureIndex)
      featureAndDataMap.put(featureValue,
          featureAndDataMap(featureValue).accumulate(point.yValue))
    }
    // Convert to a sequence of (FeatureValue, (Count, y-value sum))
    // and sort by the average y-value.
    val combinedData = new ArrayBuffer[FeatureValueAndData]
    for ((featureValue, featureValueData) <- featureAndDataMap) { 
      combinedData += new FeatureValueAndData(featureValue,
          featureValueData.featureCount, featureValueData.yValSum)
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
        sortedData(0).featureValue, 0, 0))(_ accumulate _)
    sortedData.clear()
    val predictionsAndRelError= for (index <- 1 until accumulatedData.size)
      yield PredictionAndRelativeError.compute(index, accumulatedData)
    val bestPredictionAndError = predictionsAndRelError.minBy(_.relativeError)
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
    	bestPredictionAndError.relativeError)
  }
  
  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private val data = new ArrayBuffer[Point]
}

// Calculate the best split for an ordered node.
// Ctor arguments:
//    featureIndex: the index of the FeatureValue within a Point to
//                  evaluate.
// Use this like:
//
// for (point <- allPoints) orderedNodeSplit.process(point)
// bestSplit = orderedNodeSplit.findBestSplit()
class OrderedNodeSplit(val featureIndex: Int) extends NodeSplitBase {
  override def process(point: Point): Unit = {
    data.append(point)
  }
  
  override def findBestSplit(): BestSplit = {
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
        new FeatureValueAndData(sortedData(0).features(featureIndex), 0, 0))(
            _.accumulate(featureIndex)(_))
    sortedData.clear()
    val predictionsAndRelError = for (index <- 1 until accumulatedData.size)
      yield PredictionAndRelativeError.compute(index, accumulatedData)
    val bestPredictionAndError = predictionsAndRelError.minBy(_.relativeError)
    // The 0 element is garbage.
    val splitData = accumulatedData.dropRight(
        accumulatedData.size - bestPredictionAndError.index - 1)
    val splitFeatures = new ArrayBuffer[FeatureValue]
    splitFeatures += accumulatedData(bestPredictionAndError.index).featureValue
    new BestSplit(featureIndex,
        splitFeatures,
        bestPredictionAndError.leftPrediction,
        bestPredictionAndError.rightPrediction,
        bestPredictionAndError.relativeError)
  }

  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
  
  private val data = new ArrayBuffer[Point]
  private class FeatureData(val featureValue: FeatureValue, val yValue: Double) {
  }
}