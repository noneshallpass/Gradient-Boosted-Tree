package gradientBoostedTree.tests

import gradientBoostedTree._
import org.scalatest.FunSuite
import junit.framework.Assert._
import scala.collection.mutable.ArrayBuffer

class LeafRetrainerTest extends FunSuite {

  test("Squared Loss") {
    val lossFunction: LossFunction = new SquaredLoss
    val data = for (i <- 1 to 10)
      yield new Point(Array(new FeatureValue(0)), i)
    val retrainer = LeafRetrainer.createRetrainer(lossFunction)
    data.foreach(point => retrainer.process(point))
    assert(5.5 === retrainer.getPrediction())
  }
  
  test("Absolute Loss") {
    val lossFunction: LossFunction = new AbsoluteLoss
    val data = new ArrayBuffer[Point]
    for (i <- 1 to 10) data.append(new Point(Array(new FeatureValue(0)), i))
    for (i <- 1 to 3) data.append(new Point(Array(new FeatureValue(0)), 0))    
    val retrainer = LeafRetrainer.createRetrainer(lossFunction)
    data.foreach(point => retrainer.process(point))
    // Odd # of points
    assert(4.0 === retrainer.getPrediction())
    
    // Even # of points.
    retrainer.process(new Point(Array(new FeatureValue(0)), 0))
    assert(3.5 === retrainer.getPrediction())
  }
  
  test("Huber Loss") {
    val lossFunction: LossFunction = new HuberLoss(100)
    val data = new ArrayBuffer[Point]
    for (i <- 1 to 100000)
      data.append(new Point(Array(new FeatureValue(0)), i)) 
    var retrainer = LeafRetrainer.createRetrainer(lossFunction)
    data.foreach(point => retrainer.process(point))
    assert(50000.5 === retrainer.getPrediction())
    
    data.clear
    for (i <- 1 to 1000)
      data.append(new Point(Array(new FeatureValue(0)),
          -2.0*i + Math.exp(5e-3 * i)))
    retrainer = LeafRetrainer.createRetrainer(lossFunction)
    data.foreach(point => retrainer.process(point))
    assert(Math.abs(-988.6498 - retrainer.getPrediction()) < 1e-3)
  }
}