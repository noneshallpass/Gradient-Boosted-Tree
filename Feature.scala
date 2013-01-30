package gradientBoostedTree

import scala.collection.immutable.StringOps
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

// A wrapper class for different types of feature x-values.
// FeatureValue instantiations are comparable.
class FeatureValue(val value: Any) extends Ordered[FeatureValue] {
  def isDouble: Boolean = value.isInstanceOf[Double]
  def isInt: Boolean = value.isInstanceOf[Int]
  def isString: Boolean = value.isInstanceOf[String]
  def getDouble: Double = value.asInstanceOf[Double]
  def getInt: Int = value.asInstanceOf[Int]
  def getString: String = value.asInstanceOf[String]
  
  override def compare(other: FeatureValue): Int = other.value match {
    case that: Double => getDouble.compare(that)
    case that: Int => getInt.compare(that)
    case that: String => getString.compare(that)
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

class FeatureList(val featureValues: ArrayBuffer[FeatureValue]) {  
}

// A container for the different categorical values for a Feature.
// This class requires that all FeatureValues must be of the same type, e.g. Int.
class FeatureCategories(val featureValues: ArrayBuffer[FeatureValue]) {
  private val values = featureValues.toSet[FeatureValue]
  def contains(featureValue: FeatureValue): Boolean = values.contains(featureValue)
  def getCategories() = values
}

class Point(val featureValues: ArrayBuffer[FeatureValue],
    val yValue: Double) {
}