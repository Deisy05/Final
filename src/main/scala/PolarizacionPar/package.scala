import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.{ParMap, ParSeq}
import scala.collection.{Map, Seq, mutable}
import common._

package object PolarizacionPar {
    /* -------------------------------------------------------------------------- */
    /*                       2.4.1 Función de Esteban y Rey                       */
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

    /**
    * @param d Una distribución (tupla posibles decisiones y las frecuencias asociadas a ellas)
    * @return Valor de la medida de polarización
    */
    def rhoERPar(d: DistributionPar) : Double = {
        val K = 10
        val a = 1.6
        val l = d._2.length // Los vectores deben tener la misma longitud
        val PI = d._1
        val Y = d._2

        def getSum(start : Int, end : Int) : Double = {
            println(start)
            println(end)
            (for{
                i <- start until end
                j <- 0 until l
            } yield math.pow(PI(i), 1 + a) * PI(j) * math.abs(Y(i) - Y(j))).sum
        } // math.pow(PI(i), 1 + a) * PI(j) * math.abs(Y(i) - Y(j))

        // Separamos la suma en dos
        val (sum1, sum2) = (getSum(0, l / 2), getSum(l / 2, l))

        K * (sum1 + sum2)
    }

    /* -------------------------------------------------------------------------- */
    /*                  2.4.2 Función de polarización de una red                  */
    /* -------------------------------------------------------------------------- */

    type SpecificBeliefConfPar = ParVector[Double]
    /*
        - Si b : BeliefConf, para cada i en Int, b[i] es un numero entre 0 y 1 que
        indica cuanto cree el agente i en la veracidad de la proposición p.
        - El numero de agentes es b.length
        - Si existe b(i) < 0 o b(i) > 1 esta mal definida.
        - Para i en Int / A, b(i) no tiene sentido.
    */

    type GenericBeliefConfPar = Int => SpecificBeliefConfPar
    // Si gb : GenericBeliefConf, entonces gb(n) = b tal que b : BeliefConf

    type DiscretizationPar = ParVector[Double]
    /*
        - d_k es una discretizacion de longitud k del intervalo [0,1].
        - Si d_k .length = k , 0 < d_k(i) < 1, 0 <= i <= k -1
        - d_k(i) < d_k(i + 1), 0 <= i < k - 1
    */

    /**
    * @param d_k Una discretización
    * @param sb Una creencia especifica
    * @return Valor de la polarización de ese conjunto de agentes.
    */
    def rhoPar(d_k: DiscretizationPar, sb: SpecificBeliefConfPar): Double = {
        val d_k2 = 0.0 +: d_k :+ 1.0
        val l = d_k2.length

        // Dividimos la frecuencia en dos
        def getFrequency(start : Int, end : Int) = {
            (for {
                i <- start until end
                count = (for(j <- sb if j >= d_k2(i) && j < d_k2(i + 1)) yield 1.0).sum
            } yield count / sb.length).toVector.par
        }

        val(freq1, freq2) = parallel(getFrequency(0, l / 2), getFrequency(l / 2, l - 1))
        val frequency = freq1 ++ freq2

        def getDistribution(start : Int, end : Int)= {
            (for {
                i <- start until end
            } yield (d_k2(i + 1) + d_k2(i)) / 2).toVector.par
        }

        val (distr1, distr2) = parallel(getDistribution(0, l / 2), getDistribution(l / 2, l - 1))
        val distribution = distr1 ++ distr2

        rhoERPar(frequency, distribution)
    }

    /* -------------------------------------------------------------------------- */
    /*             2.3.2 Función de actualización de una creencia                 */
    /* -------------------------------------------------------------------------- */

    type WeightedGraph = (Int, Int) => Double // Función que asocia dos agentes con la influencia del primero sobre el segundo

    type SpecificWeightedGraph = (WeightedGraph, Int) // El Int corresponde al numero de agentes que cree en una creencia

    /**
    * @param b Una creencia específica
    * @param swg Una función de influencia específica
    * @return Creencia específica.
    */
    def confBiasUpdatePar(b: SpecificBeliefConfPar, swg: SpecificWeightedGraph):
    SpecificBeliefConfPar = {
        val agentsNumber = swg._2
        val weightedGraph = swg._1

        def getSpecificBelief(start : Int, end : Int) = {
            (for {
                i <- start until end
                a_i = (for {
                    j <- 0 until agentsNumber
                    if(weightedGraph(j, i) > 0)
                } yield (1 - math.abs(b(j) - b(i))) * weightedGraph(j, i) * (b(j) - b(i)))
            } yield b(i) + (a_i.sum / a_i.length)).toVector.par
        }

        val (specificBelief1, specificBelief2) = parallel(
            getSpecificBelief(0, agentsNumber / 2),
            getSpecificBelief(agentsNumber / 2, agentsNumber))

        specificBelief1 ++ specificBelief2
    }
}
