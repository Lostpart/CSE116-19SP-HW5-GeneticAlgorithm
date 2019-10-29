package genetics.genes

/**
  * Stores a genes value as a Double in the range 0.0 - 1.0
  */
class Gene(val geneValue: Double) {

  /* You may add methods as needed */

  def decode:Double={
    //Math.tan((geneValue - 0.5)*2*1.56206969)
    (geneValue-0.5) * 100
  }


}
