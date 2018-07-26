package com.github.holothuroid.covella


/**
  * A periodic era allows for regular intercalation.
  * @param elements A sequence of ever repeating Measurables.
  */

case class PeriodicEra(elements: Seq[TimeUnit]) extends ConcreteEra {
  def subunits = elements.head.subunits
  def isDefinedAt(i: BigInt): Boolean = true
  def unitDesignation: Symbol = subunits.head

  def optimise = if (elements.toSet.size == 1) HomogeneousEra(elements.head) else this

  private def findPeriodIndex(i: BigInt) = {
    val modulus = (i % elements.size).toInt
    if (i >= 0 || modulus == 0) modulus  else elements.size  + modulus
  }

  def apply(i: BigInt): Option[TimeUnit] = Some(elements(findPeriodIndex(i)))


  def ticksOf(i: BigInt): BigInt = apply(i).get.ticks

  lazy val periodLength : BigInt = elements.map(_.ticks).sum
  def ticksOfElement : Seq[BigInt] = elements.map(_.ticks)
  lazy val ticksUntilElement : Seq[BigInt] = ticksOfElement.scanLeft(BigInt(0))(_+_)

  def ticksUntil(i: BigInt): BigInt =
    (i / elements.size                                        // Find the iteration of the period.
      - (if (i<0 && (i%elements.size).abs !=0 ) 1 else 0)) *  // With negative results that do not coincide with with period start go 1 further step back.
      periodLength +                                        // Multiply by period length to get ticks.
      ticksUntilElement( findPeriodIndex(i) )             // Add remaining ticks within the period.



  def byTicks(tocks: BigInt): Either[String, Datum] = {
    val within : BigInt = tocks/periodLength
    val periodNumber = if (tocks >= 0 || (tocks-periodLength*within).abs==0) within else within-1
    val modulo = (tocks%periodLength).abs
    val tocksInPeriod : BigInt =
      if (tocks >= 0 ) modulo
      else if (modulo== 0) 0
      else periodLength - modulo

    val fittingElement : Option[(BigInt,Int)] = ticksUntilElement.zipWithIndex.takeWhile(_._1 <= tocksInPeriod).lastOption

    fittingElement match {
      case Some((lowerBound,index))  =>
        val remainingInElement = tocksInPeriod-lowerBound
        elements(index).byTicks(remainingInElement).
          map(_ & new Datum(unitDesignation -> Ok(index + periodNumber*elements.size) ) )
      case _ =>  Left(s"Something went wrong in PeriodicEra.")
    }

  }

  override def toString = elements.mkString("Era(","\n",")")

}

object PeriodicEra{

  private[covella] def fromClauses(clauses : Seq[EraClause]) : PeriodicEra  = {
    require(clauses.map(_.condition).forall(_.isInstanceOf[PeriodicFunction]))

    val periodicConditions = clauses.map(_.condition.asInstanceOf[PeriodicFunction])

    def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b) // curiously not part of the standard library
    def lcm(a: Int, b: Int): Int = (a * b).abs / gcd(a, b)
    def lcmmult(xs: Seq[Int]): Int = xs.foldLeft(1)((x, y) => lcm(x, y))

    val numberOfPeriodElements = lcmmult(periodicConditions.map(_.baseNumber))

    val mutableArray = new Array[TimeUnit](numberOfPeriodElements)

    for (
      clause <- clauses.reverse;
      i <- 0 until numberOfPeriodElements; if clause.condition(i)
    )   mutableArray(i) = clause.value

    PeriodicEra(mutableArray.toVector)
  }
}




/**
  * PeriodicFunctions extend Function1[BigInt,Boolean].
  * When the clause conditions of a MixedEra consist exclusively of PeriodicFunction, the .optimise method will reduce the MixedEra to a PeriodicEra.
  */

trait PeriodicFunction extends Function[BigInt,Boolean] {
  def baseNumber : Int
}

case class divisibleBy(baseNumber: Int) extends  Function1[BigInt,Boolean] with PeriodicFunction  {
  def apply (x: BigInt) = x % baseNumber ==0
}

case class notDivisibleBy(baseNumber: Int) extends  Function1[BigInt,Boolean] with PeriodicFunction  {
  def apply (x: BigInt) = x % baseNumber != 0
}

case class congruent(remainder: Int, baseNumber: Int) extends  Function1[BigInt,Boolean] with PeriodicFunction  {
  def apply (x: BigInt) = (x % baseNumber).abs == remainder.abs
}

case object Pass  extends  Function1[BigInt,Boolean] with PeriodicFunction  {
  def apply (x: BigInt) = true
  val baseNumber = 1
}
