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
    assertEquals(expectedBestSplit.error, actualBestSplit.error)
  }

  test("Ordered Node Split 1") {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    // assert(nodeSplit.nodeSplit.isInstanceOf[OrderedNodeSplit])
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
        -1.0, -0.25, 2.75)
    checkBestSplit(expectedBestSplit, bestSplit)
  }
  
  test("Ordered Node Split 2") {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    // assert(nodeSplit.nodeSplit.isInstanceOf[OrderedNodeSplit])
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
        3.0, 9.0, 16.0)
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
        3.0, 9.0, 16.0)
    checkBestSplit(expectedBestSplit, bestSplit)
  }
  
  test("Split of 1 Point") {
    val featuresType = Array(new FeatureType(true, Set(new FeatureValue(0))))
    val nodeSplit = new NodeSplit(featuresType)
    // assert(nodeSplit.nodeSplit.isInstanceOf[OrderedNodeSplit])
    val points = Array(
        makePoint(5, 1)
        )
    for (point <- points) nodeSplit.process(point)
    val bestSplit = nodeSplit.findBestSplit()
    val expectedBestSplit = new BestSplit(0,
        new ArrayBuffer[FeatureValue] += new FeatureValue(5),
        1, 0, 0)
    checkBestSplit(expectedBestSplit, bestSplit)
  }
  
}