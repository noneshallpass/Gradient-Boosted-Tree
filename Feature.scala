package gradientBoostedTree

import scala.collection.immutable.StringOps
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.immutable.Set

// A wrapper class for different types of feature x-values.
// FeatureValue instantiations are comparable.
class FeatureValue(val value: Any) extends Ordered[FeatureValue] {
  def isDouble: Boolean = value.isInstanceOf[Double]
  def isInt: Boolean = value.isInstanceOf[Int]
  def isString: Boolean = value.isInstanceOf[String]
  def getDouble: Double = value.asInstanceOf[Double]
  def getInt: Int = value.asInstanceOf[Int]
  def getString: String = value.asInstanceOf[String]
  def hasSameType(that: FeatureValue): Boolean = value.getClass() == that.value.getClass()
  
  override def compare(other: FeatureValue): Int = other.value match {
    case that: Double => if (isDouble) getDouble.compare(that) else -1
    case that: Int => if (isInt) getInt.compare(that) else -1
    case that: String => if (isString) getString.compare(that) else -1
    case _ => -1
  }
  
  def canEqual(other: Any): Boolean = other.isInstanceOf[FeatureValue]
  
  override def equals(other: Any) = other match {
    case that: FeatureValue =>
      	(that canEqual this) && compare(that) == 0
    case _ => false
  }
  
  override def hashCode = value.hashCode
  
  override def toString = value.toString
}

class FeatureType(val isOrderedFeature: Boolean, values: Set[FeatureValue]) {
  def isOrdered: Boolean = isOrderedFeature
  def isCategorical: Boolean = !isOrderedFeature
  def getCategoricalValues: Set[FeatureValue] = values
  
  // Validates the feature type. For categorical features, this ensures that
  // there is at least one value and that all values are of the same type.
  // Ordered features are valid by default.
  def validate: Boolean = {
    if (isCategorical) {
      if (values.isEmpty) false
      else {
        var haveSameType = true
        var last = values.first
        for (current <- values) {
          haveSameType &&= last.hasSameType(current)
          last = current
        }
        haveSameType
      }
    }
    else true
  }
}


// A container for the different categorical values for a Feature.
// This class requires that all FeatureValues must be of the same type, e.g. Int.
class FeatureCategories(val features: ArrayBuffer[FeatureValue]) {
  private val values = features.toSet[FeatureValue]
  def contains(feature: FeatureValue): Boolean = values.contains(feature)
  def getCategories() = values
}

class Point(val features: ArrayBuffer[FeatureValue],
    val yValue: Double) {
}