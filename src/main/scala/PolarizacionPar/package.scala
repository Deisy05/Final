import Polarizacion.{Distribution, SpecificWeightedGraph}

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.{Map, Seq, mutable}

package object PolarizacionPar {



  /* -------------------------------------------------------------------------- */
  /*                       2.1.1 Función de Esteban y Rey                       */
  /* -------------------------------------------------------------------------- */


  type DistributionValuesPar = ParVector[Double] // Decisiones posibles sobre un tema
  // Tipo para los valores, reales, de una distribución

  type FrequencyPar = ParVector[Double] // Probabilidad de la decision
  /*
      - pi_k es una frecuencia de longitud k
      - Si pi_k.length = k, 0 <= pi_k(i) <= 1, 0 <= i <= k-1
      - pi_k.sum == 1
  */
  type DistributionPar = (FrequencyPar, DistributionValuesPar)

  def rhoERPar(d: DistributionPar): Double = {
    val K = 10
    val a = 1.6
    val l = d._1.length // Los vectores deben tener la misma longitud
    val PI = d._1
    val Y = d._2

    K * (for (i <- 0 until l; j <- 0 until l)
      yield math.pow(PI(i), 1 + a) * PI(j) * math.abs(Y(i) - Y(j))).sum
  }

  type SpecificBeliefConfPar = ParVector[Double]
  /*
      - Si b : BeliefConf, para cada i en Int, b[i] es un numero entre 0 y 1 que
      indica cuanto cree el agente i en la veracidad de la proposición p.
      - El numero de agentes es b.length
      - Si existe b(i) < 0 o b(i) > 1 esta mal definida.
      - Para i en Int/A, b(i) no tiene sentido.
  */

  type GenericBeliefConf = Int => SpecificBeliefConfPar
  // Si gb : GenericBeliefConf, entonces gb(n) = b tal que b : BeliefConf

  type DiscretizationPar = ParVector[Double]
  /*
      - d_k es una discretizacion de longitud k del intervalo [0,1].
      - Si d_k .length = k , 0 < d_k(i) < 1, 0 <= i <= k -1
      - d_k(i) < d_k(i + 1), 0 <= i < k - 1
  */


  def rhoPar(d_k: DiscretizationPar, sb: SpecificBeliefConfPar): Double = {
    val d_k2 = 0.0 +: d_k :+ 1.0

    val frequency = (for {
      i <- 0 until (d_k2.length - 1)
      count = (for (j <- sb if j >= d_k2(i) && j < d_k2(i + 1)) yield 1.0).sum
      // count es el numero de agentes que se encuentran en el intervalo
    } yield count / sb.length).toVector.par

    val distribution = (for (i <- 0 until (d_k2.length - 1)) yield (d_k2(i + 1) + d_k2(i)) / 2).toVector.par
    // El valor medio de cada intervalo (mid([l_i,l_s] = (l_i + l_s) / 2)
    rhoERPar(frequency, distribution)
  }
}
