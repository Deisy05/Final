package object Polarizacion {
    /* -------------------------------------------------------------------------- */
    /*                       2.1.1 Función de Esteban y Rey                       */
    /* -------------------------------------------------------------------------- */


    type DistributionValues = Vector[Double] // Decisiones posibles sobre un tema
    // Tipo para los valores, reales, de una distribución

    type Frequency = Vector[Double] // Probabilidad de la decision
    /*
        - pi_k es una frecuencia de longitud k
        - Si pi_k.lenght=k, 0 <= pi_k(i) <= 1, 0 <= i <= k-1
        - pi_k.sum == 1
    */
    type Distribution = (Frequency, DistributionValues)

    /**
    * @param d Una distribución (tupla posibles decisiones y las frecuencias asociadas a ellas)
    * @return Valor de la medida de polarización
    */
    def rhoER(d: Distribution): Double = {
        val K = 10
        val a = 1.6
        val l = d._1.length // Los vectores deben tener la misma longitud
        val PI = d._1
        val Y = d._2

        K * (for(i <- 0 until l; j <- 0 until l)
        yield math.pow(PI(i), 1 + a) * PI(j) * math.abs(Y(i) - Y(j))).sum
    }


    /* -------------------------------------------------------------------------- */
    /*                  2.2.1 Función de polarización de una red                  */
    /* -------------------------------------------------------------------------- */


    type SpecificBeliefConf = Vector[Double]
    /*
        - Si b : BeliefConf, para cada i en Int, b[i] es un numero entre 0 y 1 que
        indica cuanto cree el agente i en la veracidad de la proposición p.
        - El numero de agentes es b.length
        - Si existe b(i) < 0 o b(i) > 1 esta mal definida.
        - Para i en Int/A, b(i) no tiene sentido.
    */

    type GenericBeliefConf = Int => SpecificBeliefConf
    // Si gb : GenericBeliefConf, entonces gb(n) = b tal que b : BeliefConf

    type Discretization = Vector[Double]
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
    def rho(d_k: Discretization, sb: SpecificBeliefConf): Double = {
        val d_k2 = 0.0 +: d_k :+ 1.0

        val frequency = (for {
                i <- 0 until (d_k2.length - 1)
                count = (for(j <- sb if j >= d_k2(i) && j < d_k2(i + 1)) yield 1.0).sum
                // count es el numero de agentes que se encuentran en el intervalo
            } yield count / sb.length).toVector

        val distribution = (for(i <- 0 until (d_k2.length - 1)) yield (d_k2(i + 1) + d_k2(i)) / 2).toVector
        // El valor medio de cada intervalo (mid([l_i,l_s] = (l_i + l_s) / 2)
        rhoER(frequency, distribution)
    }


    /* -------------------------------------------------------------------------- */
    /*                         2.3.1 Función de influencia                        */
    /* -------------------------------------------------------------------------- */


    type WeightedGraph = (Int, Int) => Double // Función que asocia dos agentes con la influencia del primero sobre el segundo
    type SpecificWeightedGraph = (WeightedGraph, Int) // El Int corresponde al numero de agentes que cree en una creencia
    type GenericWeightedGraph = Int => SpecificWeightedGraph

    /**
    * @param swg Una función de influencia especifica
    * @return Matriz asociada al grafo de influencia.
    */
    def showWeightedGraph(swg : SpecificWeightedGraph) : IndexedSeq[IndexedSeq[Double]] = {
        val agentsNumber = swg._2
        val weightedGraph = swg._1
        (for{
            i <- 0 until agentsNumber

            appliedFunction = (for{
              j <- 0 until agentsNumber
            } yield weightedGraph(i, j)).toVector
        } yield appliedFunction).toVector
    }

  /* -------------------------------------------------------------------------- */
  /*             2.3.2 Función de actualización de una creencia                 */
  /* -------------------------------------------------------------------------- */

  /**
   * @param b una creencia específica
   * @param swg una función de influencia específica
   * @return creencia específica
   */
  def confBiasUpdate (b: SpecificBeliefConf, swg: SpecificWeightedGraph):
    SpecificBeliefConf= {

    val agentsNumber = swg._2
    val weightedGraph = swg._1

    (for {
      i <- 0 until agentsNumber
      a_i= (for{
        j <- 0 until agentsNumber
        if(weightedGraph(j,i)>0)
      }yield (1-math.abs(b(j)-b(i)))*weightedGraph(j,i)*(b(j)-b(i)))
    }yield b(i)+(a_i.sum/a_i.length)).toVector
  }






}