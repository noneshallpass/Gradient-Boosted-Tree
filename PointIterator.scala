package gradientBoostedTree

// An abstract class for iterating over training data.
abstract class PointIterator {
  def hasNext(): Boolean
  def next(): Point
  def reset(): Unit
}