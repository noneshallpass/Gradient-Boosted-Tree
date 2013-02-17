package gradientBoostedTree

// Training gradient boosted trees requires fitting a tree to the input
// data where y-Values are replaced by the negative derivative of a
// loss function evaluated at the training residual. This class turns
// an existing PointIterator into one which provides said modification
// to the training data.
//
// pointIterator: An iterator over training data.
// lossFunction:  A loss function, e.g. HuberLoss, SquaredLoss.
// forest:        The forest that we are currently training / growing.
class DifferentialPointIterator(
    val pointIterator: PointIterator,
    val lossFunction: LossFunction,
    val forest: Forest) extends PointIterator {

  def hasNext(): Boolean = pointIterator.hasNext()
  
  def next(): Point = {
    val point = pointIterator.next()
    val prediction: Double = forest.getPrediction(point.features)
    val newYVal = lossFunction.getNegDerivative(point.yValue - prediction)
    return new Point(point.features, newYVal)
  }
  
  def reset(): Unit = pointIterator.reset()
}