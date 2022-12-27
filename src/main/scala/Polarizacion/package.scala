
package object Polarizacion {

  //PRIMERA FUNCIÓN-------------------------------------------------------------------

  type DistributionValues = Vector[Double]
  // Tipo para los valores, reales, de una distribución

  type Frequency = Vector[Double]
  // pi_k es una frecuencia de longitud k
  // si pi_k.lenght=k, 0 <= pi_k(i) <= 1, 0 <= i <= k-1
  // pi_k.sum == 1

  type Distribution = (Frequency, DistributionValues)

  def rhoER(d: Distribution): Double = {
    val K = 10
    val a = 1.6
    val l = d._1.length
    val PI = d._1
    val Y = d._2
    K * (for(i <- 0 until l; j <- 0 until l)
      yield math.pow(PI(i), 1+a) * PI(j) * math.abs(Y(i) - Y(j))).sum
  }

  //SEGUNDA FUNCIÓN----------------------------------------------------------------------------
}