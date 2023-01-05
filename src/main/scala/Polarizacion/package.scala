
package object Polarizacion {

  //DEFINICIONES----------------------------------------------------------------------

  type DistributionValues = Vector[Double] //decisiones posibles sobre un tema
  // Tipo para los valores, reales, de una distribución

  type Frequency = Vector[Double]  //Probabilidad de la decision
  // pi_k es una frecuencia de longitud k
  // si pi_k.lenght=k, 0 <= pi_k(i) <= 1, 0 <= i <= k-1
  // pi_k.sum == 1

  type Distribution = (Frequency, DistributionValues)

  //PRIMERA FUNCIÓN-------------------------------------------------------------------

  /**
   * recibe un vector de frecuencias y un vector de valores de distribución
   * devuelve el valor de la medida de polarización
   */
  def rhoER(d: Distribution): Double = {
    val K = 10
    val alpha = 1.6
    val l = d._1.length

    val PI = d._1
    val Y = d._2

    K * (for(i <- 0 until l; j <- 0 until l)
      yield math.pow(PI(i), 1+alpha) * PI(j) * math.abs(Y(i) - Y(j))).sum
  }

  //DEFINICIONES----------------------------------------------------------------------

  type SpecificBeliefConf = Vector [ Double ] // creencia
  // Si b: BeliefConf, para cada i en Int ,
  // b[i] es un numero entre 0 y 1
  // que indica cuanto cree el agente i
  // en la veracidad de la proposición p
  // El numero de agentes es b.length
  // Si existe i: b(i) < 0 o b(i) > 1 b esta mal definida .
  // Para i en Int\A, b(i) no tiene sentido

  type GenericBeliefConf = Int => SpecificBeliefConf  //definir creencias especificas
  // si gb: GenericBeliefConf, entonces gb(n) = b
  // n agentes
  // tal que b: SpecificBeliefConf
  type Discretization = Vector [ Double ]
  // dk es una discretizacion de longitud k
  // del intervalo [0,1]
  // si, dk.lengt h=k, 0< dk(i) < 1 , 0<=i<=k−1
  // dk(i) < dk(i+1), 0 <= i < k−1

  //SEGUNDA FUNCIÓN----------------------------------------------------------------------------

  /**
   * recibe una discretización dk y una creencia específica sb
   * devuelve el valor de la polarización de ese conjunto de agentes.
   */
  def rho(d_k: Discretization, sb: SpecificBeliefConf): Double = {
    val d_k2 = 0.0 +: d_k :+ 1.0

    val frequency = (for{
      i <- 0 until d_k2.length - 1
      count = (for(j <- sb if j>=d_k2(i) && j < d_k2(i+1)) yield 1.0).sum
    } yield count/sb.length).toVector

    val distribution = (for(i <- 0 until d_k2.length - 1) yield (d_k2(i+1) + d_k2(i))/2).toVector

    rhoER(frequency, distribution)
  }














}