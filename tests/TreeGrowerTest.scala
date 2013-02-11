package gradientBoostedTree.tests

import gradientBoostedTree._
import junit.framework.Assert._
import org.scalatest.FunSuite
import scala.collection.mutable.ArrayBuffer

class TreeGrowerTest extends FunSuite {

  def makePoint(y: Double, x: Int*): Point = {
    val xvals = new Array[FeatureValue](x.length)
    for (i <- 0 until x.length) xvals(i) = new FeatureValue(x(i))
    new Point(xvals, y)
  }
  
  class TestPointIteratorDim3(val length: Int) extends PointIterator {
    override def hasNext() = nextIndex < length
   
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
  
  test("Grow Tree 1 Node 1 Node Max") {
    val tree = new Tree(1, 1)
    val featureTypes = Array(
        new FeatureType(false, Set()),
        new FeatureType(false, Set()),
        new FeatureType(false, Set())
    )
    val pointIterator = new TestPointIteratorDim3(1)
    val treeGrower = new TreeGrower(tree, featureTypes,
        new TestPointIteratorDim3(1))
    treeGrower.Grow()
    pointIterator.reset()
    assert(1 === tree.getPrediction(pointIterator.next().features))
  }
  
}