package genetics.geometry

class Line(val slope: Double, val yIntercept: Double) {

  def evaluate(y: Double): Double = {
    slope * y + yIntercept
  }

  override def toString: String = {
    f"y = $slope%1.3fx + $yIntercept%1.3f"
  }

}
