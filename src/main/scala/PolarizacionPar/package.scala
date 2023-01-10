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








}
