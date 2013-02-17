package gradientBoostedTree.tests

import gradientBoostedTree._
import org.scalatest.FunSuite
import junit.framework.Assert._
import scala.collection.immutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class NodeSplitTest extends FunSuite {

  test("FeatureValueAndData Equal") {
    assert(new FeatureValueAndData(new FeatureValue(0), 1, 2.0, 3.0) ===
      new FeatureValueAndData(new FeatureValue(0), 1, 2.0, 3.0))
  }
  
  test("FeatureValueAndData Accumulate Same Type") {
    val data = Array(
        new FeatureValueAndData(new FeatureValue(0), 1, 2.0, 4.0),
        new FeatureValueAndData(new FeatureValue(3), 2, 1.0, 1.0),
        new FeatureValueAndData(new FeatureValue(6), 4, 3.0, 9.0),
        new FeatureValueAndData(new FeatureValue(7), 3, 6.0, 36.0),
        new FeatureValueAndData(new FeatureValue(9), 5, 4.0, 16.0))
    val accumulatedData = data.scanLeft(new FeatureValueAndData(
        data(0).featureValue, 0, 0, 0))(_ accumulate _)
    val expectedData = Array(
        new FeatureValueAndData(new FeatureValue(0), 0, 0.0, 0.0),
        new FeatureValueAndData(new FeatureValue(0), 1, 2.0, 4.0),
        new FeatureValueAndData(new FeatureValue(3), 3, 3.0, 5.0),
        new FeatureValueAndData(new FeatureValue(6), 7, 6.0, 14.0),
        new FeatureValueAndData(new FeatureValue(7), 10, 12.0, 50.0),
        new FeatureValueAndData(new FeatureValue(9), 15, 16.0, 66.0)) 
    assert(expectedData === accumulatedData)
  }
  
  test("FeatureValueData Equal") {
    assert(new FeatureValueData(1, 2, 3) === new FeatureValueData(1, 2, 3))
  }
  
  test("FeatureValueData Accumulate Y") {
    val featureAndDataMap = new HashMap[FeatureValue, FeatureValueData].
      withDefaultValue(new FeatureValueData(0, 0, 0))
    val points = Array(
        makePoint(1, 2),
        makePoint(3, 5),
        makePoint(1, 6),
        makePoint(4, 8),
        makePoint(4, 7))        
    for (point <- points) {
      val featureValue = point.features(0)
      featureAndDataMap.put(featureValue,
          featureAndDataMap(featureValue).accumulate(point.yValue))
    }
    val expectedMap = Map(
        new FeatureValue(1) -> new FeatureValueData(2, 8, 40),
        new FeatureValue(3) -> new FeatureValueData(1, 5, 25),
        new FeatureValue(4) -> new FeatureValueData(2, 15, 113))
    assert(expectedMap === featureAndDataMap)
  }
  
  def makePoint(x: Int, y: Double): Point = {
    new Point(Array(new FeatureValue(x)), y)
  }
  
  def checkBestSplit(expectedBestSplit: BestSplit,
      actualBestSplit: BestSplit): Unit = {
    assertEquals(expectedBestSplit.featureIndex, actualBestSplit.featureIndex)
    assertEquals(expectedBestSplit.splitFeatures, actualBestSplit.splitFeatures)    
    assertEquals(expectedBestSplit.leftPrediction,
        actualBestSplit.leftPrediction)
    assertEquals(expectedBestSplit.rightPrediction,
        actualBestSplit.rightPrediction)
    assertEquals(expectedBestSplit.leftError, actualBestSplit.leftError)
    assertEquals(expectedBestSplit.rightError, actualBestSplit.rightError)
    assertEquals(expectedBestSplit.leftCount, actualBestSplit.leftCount)
    assertEquals(expectedBestSplit.rightCount, actualBestSplit.rightCount)
  }

  test("Ordered Node Split 1") {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    val points = Array(
        makePoint(5, 1),
        makePoint(1, -1),
        makePoint(3, -1),
        makePoint(10, 0),
        makePoint(7, -1),
        makePoint(9, -1)
        )
    for (point <- points) nodeSplit.process(point)
    val bestSplit = nodeSplit.findBestSplit()
    val expectedBestSplit = new BestSplit(0,
        new ArrayBuffer[FeatureValue] += new FeatureValue(3),
        -1.0, -0.25, 0.0, 2.75, 2, 4)
    checkBestSplit(expectedBestSplit, bestSplit)
  }
  
  test("Ordered Node Split 2") {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    val points = Array(
        makePoint(1, 1),
        makePoint(3, 5),
        makePoint(5, 7),
        makePoint(7, 9),
        makePoint(9, 11))
    for (point <- points) nodeSplit.process(point)
    val bestSplit = nodeSplit.findBestSplit()
    val expectedBestSplit = new BestSplit(0,
        new ArrayBuffer[FeatureValue] += new FeatureValue(3),
        3.0, 9.0, 8.0, 8.0, 2, 3)
    checkBestSplit(expectedBestSplit, bestSplit)
  }

  test("Categorical Node Split 1") {
    val featuresType = Array(new FeatureType(false,
        Set(new FeatureValue(1),
            new FeatureValue(3),
            new FeatureValue(11))))
    val nodeSplit = new NodeSplit(featuresType)
    val points = Array(
        makePoint(1, 1),
        makePoint(3, 5),
        makePoint(5, 7),
        makePoint(7, 9),
        makePoint(9, 11))
    for (point <- points) nodeSplit.process(point)
    val bestSplit = nodeSplit.findBestSplit()
    val expectedBestSplit = new BestSplit(0,
        new ArrayBuffer[FeatureValue] += new FeatureValue(1) +=
          new FeatureValue(3),
        3.0, 9.0, 8.0, 8.0, 2, 3)
    checkBestSplit(expectedBestSplit, bestSplit)
  }
  
  test("Split of 1 Point - No Solution") {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    val points = Array(
        makePoint(5, 1)
        )
    for (point <- points) nodeSplit.process(point)
    val bestSplit = nodeSplit.findBestSplit()
    checkBestSplit(BestSplit.noSolution, bestSplit)
  }
  
  test("Split of 2 points - No Error") {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    val points = Array(
        makePoint(1, 1),
        makePoint(3, 5))
    for (point <- points) nodeSplit.process(point)
    val bestSplit = nodeSplit.findBestSplit()
    val expectedBestSplit = new BestSplit(0,
        new ArrayBuffer[FeatureValue] += new FeatureValue(1),
        1.0, 5.0, 0, 0, 1, 1)
    checkBestSplit(expectedBestSplit, bestSplit)
  }

  test("Split of 3 points - Error 1 side") {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    val points = Array(
        makePoint(3, 5),
        makePoint(2, 4),
        makePoint(1, 1))
    for (point <- points) nodeSplit.process(point)
    val bestSplit = nodeSplit.findBestSplit()
    val expectedBestSplit = new BestSplit(0,
        new ArrayBuffer[FeatureValue] += new FeatureValue(1),
        1.0, 4.5, 0, 0.5, 1, 2)
    checkBestSplit(expectedBestSplit, bestSplit)
  }
  
  class NodeError extends NodeSplitBase {
    override def findBestSplit(featureIndex: Int,
        data: ArrayBuffer[Point]): BestSplit = null
    
    def makeFVD(featureValue: Int, featureCount: Int,
        yValSum: Double, ySqValSum: Double): FeatureValueAndData = {
      new FeatureValueAndData(new FeatureValue(featureValue), featureCount,
          yValSum, ySqValSum)
    }
    
    def makePAE(index: Int, leftError: Double,
        rightError: Double): PredictionAndError = {
      new PredictionAndError(index, 0, 0, leftError, rightError)
    }
    
    val accumulatedData = ArrayBuffer(
        makeFVD(0, 0, 0, 0),
        makeFVD(1, 0, 0, 0),
        makeFVD(2, 0, 0, 0),
        makeFVD(2, 0, 0, 0),
        makeFVD(2, 0, 0, 0),
        makeFVD(3, 0, 0, 0),
        makeFVD(4, 0, 0, 0))
        
    val predictionAndError = Array(
        makePAE(1, 1, 1),
        makePAE(2, 0, 0.2),
        makePAE(2, 0, 0),
        makePAE(2, 0.2, 0),
        makePAE(3, 0.5, 0.5),
        makePAE(4, 0.5, 0.5)
        )
    
    def getMinError = minError(accumulatedData, predictionAndError.toIndexedSeq)
  }
  
  test("Min Error") {
    val nodeError = new NodeError
    val predictionAndError = nodeError.getMinError
    assertEquals(0.2, predictionAndError.leftError)
    assertEquals(0.0, predictionAndError.rightError)    
  }
  
}