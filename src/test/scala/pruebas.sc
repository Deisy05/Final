import Polarizacion._
import PolarizacionPar._
import org.scalameter._

import scala.collection.parallel.immutable.ParVector
import scala.util.Random

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

val distri = distribuciones(200)
val frec = frecuencias(200)
val distriPar = distribucionesPar(10000)
val frecPaR = frecuenciasPar(10000)

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





/* pruebas para la función rho
val d1 = Vector(0.2, 0.4, 0.6, 0.8)
val d1Par = ParVector(0.2, 0.4, 0.6, 0.8)

def b1(nags:Int):SpecificBeliefConf= {
  Vector.tabulate(nags)((i: Int) => {if (i <= nags / 2) 0.6 else 0.4})
}

def b1Par(nags: Int): SpecificBeliefConfPar = {
  ParVector.tabulate(nags)((i: Int) => {if (i <= nags / 2) 0.6 else 0.4})
}

val b1_1000 = b1(3000000)
val b1Par_1000 = b1Par(3000000)

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
