package gradientBoostedTree.tests

import gradientBoostedTree._
import org.scalatest.FunSuite
import junit.framework.Assert._

class LossFunctionTest extends FunSuite {

  test("Squared loss") {
    val loss: LossFunction = new SquaredLoss
    assert(0.0 === loss.getLoss(0.0))
    assert(2.0 === loss.getLoss(2.0))
    assert(2.0 === loss.getLoss(-2.0))
    assert(8.0 === loss.getLoss(-4.0))

    assert(0.0 === loss.getNegDerivative(0.0)) 
    assert(2.0 === loss.getNegDerivative(2.0))
    assert(-2.0 === loss.getNegDerivative(-2.0))
    assert(-4.0 === loss.getNegDerivative(-4.0))
  }
  
  test("Absolute loss") {
    val loss: LossFunction = new AbsoluteLoss
    assert(0.0 === loss.getLoss(0.0))
    assert(2.0 === loss.getLoss(2.0))
    assert(2.0 === loss.getLoss(-2.0))
    assert(4.0 === loss.getLoss(-4.0))
 
    assert(0.0 === loss.getNegDerivative(0.0)) 
    assert(1.0 === loss.getNegDerivative(2.0))
    assert(-1.0 === loss.getNegDerivative(-2.0))
    assert(-1.0 === loss.getNegDerivative(-4.0))
  }
  
  test("Huber loss") {
    val loss: LossFunction = new HuberLoss(2.0)
    assert(0.0 === loss.getLoss(0.0))
    assert(2.0 === loss.getLoss(2.0))
    assert(2.0 === loss.getLoss(-2.0))
    assert(6.0 === loss.getLoss(4.0))    
    assert(6.0 === loss.getLoss(-4.0))

    assert(0.0 === loss.getNegDerivative(0.0))    
    assert(2.0 === loss.getNegDerivative(2.0))
    assert(-2.0 === loss.getNegDerivative(-2.0))
    assert(2.0 === loss.getNegDerivative(4.0))
    assert(-2.0 === loss.getNegDerivative(-4.0))
  }  
}