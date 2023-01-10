import Polarizacion._
import PolarizacionPar._
import org.scalameter._

import scala.collection.parallel.immutable.ParVector
import scala.util.Random

val d1 = Vector(0.2, 0.4, 0.6, 0.8)
val d1Par = ParVector(0.2, 0.4, 0.6, 0.8)

def distribuciones(tamanioVector: Int): DistributionValues = {
  Vector.tabulate(tamanioVector)((i: Int) => Random.nextDouble())
}

def frecuencias(tamanioVector: Int): Frequency = {
  Vector.tabulate(tamanioVector)((i: Int) => 100 * Random.nextDouble())
}

def distribucionesPar(tamanioVector: Int): DistributionValuesPar = {
  ParVector.tabulate(tamanioVector)((i: Int) => Random.nextDouble())
}

def frecuenciasPar(tamanioVector: Int): FrequencyPar = {
  ParVector.tabulate(tamanioVector)((i: Int) => 100 * Random.nextDouble())
}

def b1(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => {if (i <= nags / 2) 0.6 else 0.4})
}

def b1Par(nags: Int): SpecificBeliefConfPar = {
  ParVector.tabulate(nags)((i: Int) => {if (i <= nags / 2) 0.6 else 0.4})
}

def i1(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) => if (i==j) 1.0
  else if (i<j) 1.0 / (j - i).toDouble
  else 0.0, nags)
}


//val b1_1000 = b1(3000000)
//val b1Par_1000 = b1Par(3000000)
//
//
//
//val distri = distribuciones(7000)
//val frec = frecuencias(7000)
//val distriPar = distribucionesPar(7000)
//val frecPaR = frecuenciasPar(7000)

//---------------------------------------------------------------------------
/* Pruebas para la función rhoEr
val timeA1 = config(
  KeyValue(Key.exec.minWarmupRuns -> 5),
  KeyValue(Key.exec.maxWarmupRuns -> 5),
  KeyValue(Key.verbose -> false)
) withWarmer (new Warmer.Default) measure (rhoER( (frec, distri) ))

val timeA2 = config(
  KeyValue(Key.exec.minWarmupRuns -> 5),
  KeyValue(Key.exec.maxWarmupRuns -> 5),
  KeyValue(Key.verbose -> false)
) withWarmer (new Warmer.Default) measure (rhoERPar((frecPaR, distriPar)))
*/

//------------------------------------------------------------------
/* pruebas para la función rho
val timeA1 = config(
  KeyValue(Key.exec.minWarmupRuns -> 10),
  KeyValue(Key.exec.maxWarmupRuns -> 10),
  KeyValue(Key.verbose -> false)
) withWarmer (new Warmer.Default) measure (rho(d1, b1_1000))

val timeA2 = config(
  KeyValue(Key.exec.minWarmupRuns -> 10),
  KeyValue(Key.exec.maxWarmupRuns -> 10),
  KeyValue(Key.verbose -> false)
) withWarmer (new Warmer.Default) measure (rhoPar(d1Par, b1Par_1000))*/
//--------------------------------------------------------------------------------

val b_10 = b1(12000)
val b_10Par = b1Par(12000)
val i_10 = i1(12000)

val timeA1 = config(
  KeyValue(Key.exec.minWarmupRuns -> 10),
  KeyValue(Key.exec.maxWarmupRuns -> 10),
  KeyValue(Key.verbose -> false)
) withWarmer (new Warmer.Default) measure (confBiasUpdate(b_10, i_10))

val timeA2 = config(
  KeyValue(Key.exec.minWarmupRuns -> 10),
  KeyValue(Key.exec.maxWarmupRuns -> 10),
  KeyValue(Key.verbose -> false)
) withWarmer (new Warmer.Default) measure (confBiasUpdatePar(b_10Par, i_10))