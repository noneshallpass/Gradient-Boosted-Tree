package gradientBoostedTree

// TODO: fill this in.
class NodeStats {
  
}

abstract class LossFunction {
	def getLoss(nodeStats: NodeStats): Double
}

// TODO: should not be abstract
abstract class HuberLossFunction(val delta: Double) extends LossFunction {
  def getLoss(nodeStats: NodeStats): Double
  
}