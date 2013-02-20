package gradientBoostedTree

import scala.collection.mutable.ArrayBuffer
import scala.Math

// In the training of gradient boosted trees, after a tree is trained, the
// predictions within each tree leaf are re-calculated to minimize the loss.
// This provides functionality to do so.
abstract class LeafRetrainer {

  def process(point: Point): Unit
  
  // Requires process(...) was previously called at least once.
  def getPrediction(): Double
  
  // **************************************************************************
  //
  // Protected
  //
  // **************************************************************************
  
  // The median of medians algorithm is faster albeit noisy. We opt for the
  // more accurate version.
  protected def getMedian(data: ArrayBuffer[Double]): Double = {
    val sortedData = data.sortWith(_ < _)
    val mid = sortedData.size/2
    if (sortedData.size % 2 == 1) sortedData(mid)
    else (sortedData(mid) + sortedData(mid - 1)) / 2.0
  }
}

object LeafRetrainer {
  
  // From a loss function, create it's corresponding retrainer.
  def createRetrainer(lossFunction: LossFunction): LeafRetrainer = lossFunction match {
    case lossFunction: SquaredLoss => new SquaredLossRetrainer
    case lossFunction: AbsoluteLoss => new AbsoluteLossRetrainer
    case lossFunction: HuberLoss =>
      new HuberLossRetrainer(lossFunction.getParameter, lossFunction)
    case _ => null
  }
}

// For a Squared Error loss, the average data value minimizes loss.
class SquaredLossRetrainer extends LeafRetrainer {
  
  override def process(point: Point): Unit = {
    sum += point.yValue
    count += 1
  }
  
  override def getPrediction(): Double = {
    sum / count
  }
  
  private var sum: Double = 0.0;
  private var count: Integer = 0;
}

// For an Absolute Error loss, the median data value minimizes loss.
class AbsoluteLossRetrainer extends LeafRetrainer {
  
  override def process(point: Point): Unit = {
    data.append(point.yValue)
  }
  
  override def getPrediction(): Double = {
    val sortedData = data.sortWith(_ < _)
    val mid = sortedData.size/2
    if (sortedData.size % 2 == 1) sortedData(mid)
    else (sortedData(mid) + sortedData(mid - 1)) / 2.0
  }
  
  private val data = new ArrayBuffer[Double]
}

// delta: The residual value determining the boundary between the squared and
//        linear loss regions.
class HuberLossRetrainer(val delta: Double, val lossFunction: LossFunction) extends LeafRetrainer {
  
  override def process(point: Point): Unit = {
    data.append(point.yValue)
  }
  
  // Solved via gradient descent with the starting value as the median. If
  // gradient descent does not finish within 1000 iterations, the median value
  // is used instead.
  override def getPrediction(): Double = {
   val kMaxIterations: Int = 1000
   val kStoppingUpdate: Double = 1e-6
   val deltaUpdate: Double = delta / 10 / data.length
   val median = getMedian(data)
   var yEst: Double = median
   var updateRatio: Double = Double.PositiveInfinity
   var i: Int = 0
   while (Math.abs(updateRatio) > kStoppingUpdate && i < kMaxIterations) {
     var deriv: Double = 0
     data.foreach(yAct => deriv += lossFunction.getNegDerivative(yAct - yEst))
     val update = deltaUpdate * deriv
     updateRatio = if (Math.abs(yEst) > kStoppingUpdate) update / yEst
       else update / (yEst + 1)
     yEst += update
     i += 1
   }
   if (i == kMaxIterations) median
   else yEst
  }

  // **************************************************************************
  //
  // Private
  //
  // **************************************************************************
    
  private val data = new ArrayBuffer[Double]
}