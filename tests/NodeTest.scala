package gradientBoostedTree.tests

import gradientBoostedTree._
import org.scalatest.FunSuite
import junit.framework.Assert._
import scala.collection.mutable.ArrayBuffer

class NodeTest extends FunSuite {

  test("Ordered Node - Root Only") {
    val head = new OrderedNode(0, 0, 0.5)

    // Check the id assignments along with child getting
    assert(head.getId === 0)
    
    // Check isLeaf
    assert(head.isLeaf)
    
    // Check isEmptyNode
    assertFalse(head.isEmptyNode) 
    
    // Node 0
    assert(head.getPrediction(Array()) === 0.5)
  }

  // Create a function which makes Nodes. If isOrdered is true,
  // the resulting function is a proxy for an OrderedNode ctor; otherwise the
  // resulting function is a proxy for a CategoricalNode ctor.
  def makeNode(isOrdered: Boolean): (Int, Int, Double) => Node = {
    def makeNodeHelper(isOrdered: Boolean)
        (id: Int, featureIndex: Int, prediction: Double) = {
      if (isOrdered) new OrderedNode(id, featureIndex, prediction)
      else new CategoricalNode(id, featureIndex, prediction)
    }
    makeNodeHelper(isOrdered)
  }
  
  // Create a tree like the following and check it's usage.
  //      0
  //     / \
  //    1   2
  //   / \
  //  3   4
  //
  // makeNode: a proxy for a Node ctor aka the function of the same name.
  // node0Split: Passed directly to the Node 0 ctor. Determines the node split
  //             value.
  // node1Split: Passed directly to the Node 1 ctor. Determines the node split
  //             value.
  // right:     An array of feature vectors which descend to node 2.
  // leftLeft:  An array of feature vectors which descend to node 3.
  // leftRight: An array of feature vectors which descend to node 4.  
  def BuildAndCheckNode(
      makeNode: (Int, Int, Double) => Node,
      node0Split: ArrayBuffer[FeatureValue],
      node1Split: ArrayBuffer[FeatureValue],
      right: Array[Array[FeatureValue]],
      leftLeft: Array[Array[FeatureValue]],
      leftRight: Array[Array[FeatureValue]]): Unit = {
    val head = makeNode(0, 0, 0.5)
    val headLeft = makeNode(1, 1, 1.5)
    val headRight = makeNode(2, 1, 2.5)
    head.insertChildren(headLeft, headRight, node0Split)
    val headLeftLeft = makeNode(3, 2, 3.5)
    val headLeftRight = makeNode(4, 2, 4.5)
    headLeft.insertChildren(headLeftLeft, headLeftRight, node1Split)

    // Check the id assignments along with child getting
    assert(headLeft.getId === head.getLeftChild.getId)
    assert(headRight.getId === head.getRightChild.getId)
    assert(headLeftLeft.getId === head.getLeftChild.getLeftChild.getId)
    assert(headLeftRight.getId === head.getLeftChild.getRightChild.getId)

    // Check isLeaf
    assertFalse(head.isLeaf)
    assertFalse(head.getLeftChild.isLeaf)
    assert(head.getRightChild.isLeaf)    
    assert(head.getLeftChild.getLeftChild.isLeaf) 
    assert(head.getLeftChild.getRightChild.isLeaf)

    // Check isEmptyNode
    assertFalse(head.isEmptyNode)
    assertFalse(head.getLeftChild.isEmptyNode)
    assertFalse(head.getRightChild.isEmptyNode)    
    assertFalse(head.getLeftChild.getLeftChild.isEmptyNode) 
    assertFalse(head.getLeftChild.getRightChild.isEmptyNode)   

    // Node 2
    for (r <- right) assertEquals(2.5, head.getPrediction(r))
      
    // Node 3
    for (ll <- leftLeft) assertEquals(3.5, head.getPrediction(ll))
     
    // Node 4
    for (lr <- leftRight) assertEquals(4.5, head.getPrediction(lr))
    
    // Check Insufficiently Filled vectors, e.g. only descend to node 1
    for (ll <- leftLeft) assertEquals(0.0, head.getPrediction(Array(ll(0))))
  }

  test("Ordered Node") {
    // Integer Ordered tree
    BuildAndCheckNode(
        makeNode(true),  // Ordered node
        ArrayBuffer(new FeatureValue(10)),
        ArrayBuffer(new FeatureValue(40)),
        Array(
            Array(new FeatureValue(15))),  // right
        Array(
            Array(new FeatureValue(5),
                new FeatureValue(35))),    // leftLeft
        Array(
            Array(new FeatureValue(5),
                new FeatureValue(45))))   // leftRight
    println("YO YO YO")       
    // Double Ordered tree
    BuildAndCheckNode(
        makeNode(true),  // Ordered node
        ArrayBuffer(new FeatureValue(10.0)),
        ArrayBuffer(new FeatureValue(40.0)),
        Array(
            Array(new FeatureValue(15.0))),  // right
        Array(
            Array(new FeatureValue(5.0),
                new FeatureValue(35.0))),    // leftLeft
        Array(
            Array(new FeatureValue(5.0),
                new FeatureValue(45.0))))    // leftRight
            
    // String Ordered tree
    BuildAndCheckNode(
        makeNode(true),  // Ordered node
        ArrayBuffer(new FeatureValue("ab")),
        ArrayBuffer(new FeatureValue("de")),
        Array(
            Array(new FeatureValue("ac"))),  // right
        Array(
            Array(new FeatureValue("aa"),
                new FeatureValue("dc"))),    // leftLeft
        Array(
            Array(new FeatureValue("aa"),
                new FeatureValue("df"))))    // leftRight
  }
  
  test("Categorical Node") {
    // Integer Category tree
    BuildAndCheckNode(
        makeNode(false),  // Categorical node
        ArrayBuffer(new FeatureValue(1),
            new FeatureValue(2)),  // Node 0 split
        ArrayBuffer(new FeatureValue(3),
            new FeatureValue(4)),  // Node 1 split
        Array(
            Array(new FeatureValue(3)),
            Array(new FeatureValue(4))),  // right
        Array(
            Array(new FeatureValue(1),
                new FeatureValue(3)),
             Array(new FeatureValue(2),
                new FeatureValue(4))),   // leftLeft
        Array(
            Array(new FeatureValue(1),
                new FeatureValue(1))))   // leftRight
                
    // String Category tree
    BuildAndCheckNode(
        makeNode(false),  // Categorical node
        ArrayBuffer(new FeatureValue("a"),
            new FeatureValue("b")),  // Node 0 split
        ArrayBuffer(new FeatureValue("c"),
            new FeatureValue("d")),  // Node 1 split
        Array(
            Array(new FeatureValue("c")),
            Array(new FeatureValue("d"))),  // right
        Array(
            Array(new FeatureValue("a"),
                new FeatureValue("d")),
             Array(new FeatureValue("b"),
                new FeatureValue("c"))),   // leftLeft
        Array(
            Array(new FeatureValue("a"),
                new FeatureValue("a"))))   // leftRight      
  }
  
  test("Unstructured Node") {
    val nodeMaker = makeNode(true)
    val head = nodeMaker(0, 0, 0.5)
    val headLeft = new UnstructuredNode(1.5, head)
    val headRight = new UnstructuredNode(2.5, head)
    head.insertChildren(headLeft, headRight, ArrayBuffer(new FeatureValue(10)))
    val newLeftNode = nodeMaker(3, 1, 3.5)
    val newRightNode = nodeMaker(4, 1, 4.5)
    
    val leftFeatures = Array(new FeatureValue(5))
    val rightFeatures = Array(new FeatureValue(15))
    
    assert(1.5 === head.getPrediction(leftFeatures))
    assert(2.5 === head.getPrediction(rightFeatures))
    
    headLeft.replaceNode(newLeftNode)
    assert(3.5 === head.getPrediction(leftFeatures))
    assert(2.5 === head.getPrediction(rightFeatures))       
    
    headRight.replaceNode(newRightNode)
    assert(3.5 === head.getPrediction(leftFeatures))
    assert(4.5 === head.getPrediction(rightFeatures))      
  }
}