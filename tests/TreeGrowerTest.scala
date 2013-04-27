package gradientBoostedTree.tests

import gradientBoostedTree._
import junit.framework.Assert._
import org.scalatest.FunSuite
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.immutable.Set

class TreeGrowerTest extends FunSuite {

  // A helper function for creating a point with an arbitrary number of
  // feature / x-values. Use like this:
  // 
  // val point: Point = makePoint(3, 0, 1, 2)
  // val point2: Point = makePoint(4, 0, 1)
  def makePoint(y: Double, x: Int*): Point = {
    val xvals = new Array[FeatureValue](x.length)
    for (i <- 0 until x.length) xvals(i) = new FeatureValue(x(i))
    new Point(xvals, y)
  }
  
  // A PointIterator for test tree data.
  class TestPointIteratorDim3(val length: Int) extends PointIterator {
    override def hasNext() = nextIndex < Math.min(length, points.length)
   
    override def next(): Point = {
      val point = points(nextIndex)
      nextIndex += 1
      point
    }
   
    override def reset(): Unit = nextIndex = 0
   
    private val points = Array(
        makePoint(1, 2, 3, 1),
        makePoint(0, 1, 5, 4),
        makePoint(5, 10, 3, 7),
        makePoint(-2, 8, 2, 5),
        makePoint(10, 3, 5, 6),
        makePoint(15, 8, 9, 4),
        makePoint(4, 1, 5, 3),
        makePoint(2, 1, 3, 2)
        )

    private var nextIndex: Int = 0
  }
  
  // A test for grow a tree so that it has one point per node. This means
  // that we grow a tree to perfectly fit the data. Train on a subset of
  // the TestPointIteratorDim3 data as specified by numPoints.
  def testGrowTreeOnePointPerNode(numPoints: Int, isOrdered: Boolean): Unit = {
    val tree = new Tree(1, 100)
    val featureTypes = Array(
        new FeatureType(isOrdered, Set()),
        new FeatureType(isOrdered, Set()),
        new FeatureType(isOrdered, Set())
    )
    val pointIterator = new TestPointIteratorDim3(numPoints)
    val treeGrower = new TreeGrower(tree, featureTypes, pointIterator)
    treeGrower.Grow()
    pointIterator.reset()
    // Check that the tree has no training error over the data.
    while (pointIterator.hasNext()) {
      val point: Point = pointIterator.next()
      assert(point.yValue === tree.getPrediction(point.features))
    }   
  }

  test("Grow Tree - One Point Per Node") {
    for (i <- 1 to 8) {
      // All ordered nodes.
      testGrowTreeOnePointPerNode(i, true)
      // All categorical nodes.
      testGrowTreeOnePointPerNode(i, false)      
    }
  }
  
  test("Grow Tree - Single Node") {
    val tree = new Tree(1, 1)
    val featureTypes = Array(
        new FeatureType(true, Set()),
        new FeatureType(true, Set()),
        new FeatureType(true, Set())
    )
    val pointIterator = new TestPointIteratorDim3(8)
    val treeGrower = new TreeGrower(tree, featureTypes, pointIterator)
    treeGrower.Grow()
    pointIterator.reset()
    // Check that a single node tree has the average y-value = 35/8
    while (pointIterator.hasNext()) {
      val point: Point = pointIterator.next()
      assert(4.375 === tree.getPrediction(point.features))
    } 
  }
}