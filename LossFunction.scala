package gradientBoostedTree

class NodeStats {
  
}

abstract class LossFunction {
	def getLoss(nodeStats: NodeStats): Double
}

class HuberLossFunction(val delta: Double) extends LossFunction {
  def getLoss(nodeStats: NodeStats): Double
  
}