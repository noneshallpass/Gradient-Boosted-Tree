package gradientBoostedTree.tests

import gradientBoostedTree._
import org.scalatest.FunSuite
import junit.framework.Assert._

class FeatureTest extends FunSuite {
  
  test("Same Integers Equal") {
    val feature = new FeatureValue(3)
    assert(new FeatureValue(3) === new FeatureValue(3))
    assert(new FeatureValue(3) === feature)
    assert(feature === new FeatureValue(3))
  }
  
  test("Different Integers Not Equal") {
    val feature = new FeatureValue(3)
    assertFalse("Integers Not Equal", feature == new FeatureValue(2))
  }

  test("Different Types Not Equal") {
    assertFalse("String and Integer Not Equal",
        new FeatureValue("a") == new FeatureValue(2))
    assertFalse("String and Double Not Equal",
        new FeatureValue("abc") == new FeatureValue(3.0))
    assertFalse("Integer and Double Not Equal",
        new FeatureValue(3) == new FeatureValue(2.0))        
  }
  
  def comparisonTest[A](smaller: A, larger: A): Unit = {
    assert(new FeatureValue(smaller) <= new FeatureValue(smaller))
    assert(new FeatureValue(smaller) >= new FeatureValue(smaller))    
    assert(new FeatureValue(smaller) < new FeatureValue(larger))
    assert(new FeatureValue(smaller) <= new FeatureValue(larger))    
    assert(new FeatureValue(larger) > new FeatureValue(smaller))
    assert(new FeatureValue(larger) >= new FeatureValue(smaller))    
  }
  
  test("Integer Comparison") {
    comparisonTest(1, 2)
  }
  
  test("Double Comparison") {
    comparisonTest(1.0, 2.0)
  }
  
  test("String comparison") {
    comparisonTest("abc", "abd")
  }
  
  test("Categorical Feature Type") {
    val categories = Set(new FeatureValue(1),
        new FeatureValue(2),
        new FeatureValue(3))
    val categoricalType = new FeatureType(false, categories)
    assertTrue(categoricalType.isCategorical)
    assertFalse(categoricalType.isOrdered)
    assertEquals(categories, categoricalType.getCategoricalValues)
    assert(categoricalType.validate)
  }
  
  test("Categorical Feature Type Validation Fail") {
    val categories = Set(new FeatureValue(1),
        new FeatureValue(2.0),
        new FeatureValue(3))
    val categoricalType = new FeatureType(false, categories)
    assertTrue(categoricalType.isCategorical)
    assertFalse(categoricalType.isOrdered)
    assertFalse(categoricalType.validate)
  }  
  
  test("Ordered Feature Type") {
    val orderedType = new FeatureType(true, null)
    assertFalse(orderedType.isCategorical)
    assert(orderedType.isOrdered)
    assert(orderedType.validate)
  }  
}