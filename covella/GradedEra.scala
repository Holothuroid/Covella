package com.github.holothuroid.covella

object GradedEra{
  def startingWith(era : Era) = GradedEra(Vector(era.optimise),Vector())
}


/**
  * This class allows combining several eras at fixed breaking points.
  * Use this option, when you have a reform in calendar systems or similar switch.
  *
  * Best constructed with:
  * `GradedEra startingWith $era0 beginning $number1 have $era1 ... `
  */

case class GradedEra private[covella] (eras: Vector[Era], upperBounds : Vector[BigInt]) extends ConcreteEra {

  require ( eras.head.subunits == eras.last.subunits )

  def beginning(i: BigInt) =
    new {
      require(upperBounds.isEmpty || i > upperBounds.last)
      def have(era: Era) =  GradedEra(eras = eras :+ era.optimise, upperBounds = upperBounds :+ i)
      def have(tu: TimeUnit) = GradedEra(eras = eras :+ HomogeneousEra(tu), upperBounds = upperBounds :+ i)
    }


   def findEraNumberByIndex(i: BigInt) : Int = upperBounds.zipWithIndex.find(_._1 >=i).map(_._2).getOrElse(eras.size-1)
   def findEraByIndex(i: BigInt) : Era = eras(findEraNumberByIndex(i))

   def findLowerBoundByTocks(tocks: BigInt) : Option[BigInt]  =  upperBounds.takeWhile(_ <= tocks).lastOption
   def findUpperBoundByTocks(tocks: BigInt) : Option[BigInt]  =  upperBounds.find(_>=tocks)

   def ticksOfEra(eraIndex: Int) : Option[BigInt] =
    if (eraIndex == 0 || eraIndex >= eras.size) None
    else {
      val era = eras(eraIndex)
      Some ( (era.ticksUntil(upperBounds(eraIndex)) - era.ticksUntil(upperBounds(eraIndex-1))).abs)
    }

   def centralEraNumber : Int = findEraNumberByIndex(0)
   def centralEra : Era = findEraByIndex(0)
   def centralEraUpper : Option[BigInt] = findUpperBoundByTocks(0)
   def centralEraLower : Option[BigInt] = findLowerBoundByTocks(0)

   def laterEras : Vector[(BigInt,Int)] /* Lower Boundary and EraIndex */ =
    (centralEraNumber+1 until eras.size).map(x => (upperBounds(x-1),x)).toVector

   def earlierEras : Vector[(BigInt,Int)] =
    (0 until centralEraNumber).map(x => (upperBounds(x),x)).toVector

   def ticksUntilCentralEraUppper : Option[BigInt] = centralEraUpper.map(centralEra.ticksUntil)
   def ticksUntilCentralEraLower : Option[BigInt] = centralEraLower.map(centralEra.ticksUntil)

   def findEraNumberByTocks(tocks: BigInt) : Int =
    if ((ticksUntilCentralEraUppper.isEmpty || tocks < ticksUntilCentralEraUppper.get) && (
      ticksUntilCentralEraLower.isEmpty || tocks >= ticksUntilCentralEraLower.get ))
      centralEraNumber
    else if (tocks >0) {
      ticksUntilCentralEraUppper
      ???
    }
    else ???

   def findEraByTocks(tocks: BigInt): Era = eras(findEraNumberByTocks(tocks))



  override def ticksUntil(i: BigInt) : BigInt =
    if ( ( centralEraLower.isEmpty || i >= centralEraLower.get ) && ( centralEraUpper.isEmpty || i < centralEraUpper.get ) )
      centralEra.ticksUntil(i)
    else if(i>=0) ( centralEra.ticksUntil(centralEraUpper.get)
      + laterEras.filter(_._1 <= i).map(x => ticksOfEra(x._2).get).sum  +
      (findEraByIndex(i).ticksUntil(i) - findEraByIndex(i).ticksUntil(findLowerBoundByTocks(i).get)).abs   )
    else  ( centralEra.ticksUntil(centralEraLower.get)  +
      earlierEras.filter(_._1 >= i).map(x => ticksOfEra(x._2).get).sum  +
    (findEraByIndex(i).ticksUntil(i) - findEraByIndex(i).ticksUntil(findLowerBoundByTocks(i).get)).abs   )




  def byTicks(tocks: BigInt) : Either[String,Datum] = {
    if ((ticksUntilCentralEraLower.isEmpty || tocks >= ticksUntilCentralEraLower.get) &&
      (ticksUntilCentralEraUppper.isEmpty || tocks < ticksUntilCentralEraUppper.get))
      centralEra.byTicks(tocks)
    else if (tocks >= 0) {
      val targetEra = findEraByTocks(tocks)
      val gradedCounting: BigInt = ticksUntilCentralEraUppper.get +
        (centralEraNumber+1 until findEraNumberByTocks(tocks)).map(ticksOfEra).map(_.get).sum
      val tocksForTargetEra: BigInt = tocks - gradedCounting + targetEra.ticksUntil(findLowerBoundByTocks(tocks).get)

      targetEra.byTicks(tocksForTargetEra)
    }
    else {
      val targetEra = findEraByTocks(tocks)
      val gradedCounting: BigInt = ticksUntilCentralEraLower.get +
        (findEraNumberByTocks(tocks)+1 until centralEraNumber) .map(ticksOfEra).map(_.get).sum
      val tocksForTargetEra: BigInt = tocks - gradedCounting + targetEra.ticksUntil(findUpperBoundByTocks(tocks).get)

      targetEra.byTicks(tocksForTargetEra)

    }
  }

  override def subunits: Seq[Symbol] = eras.head.subunits
  override def optimise: Era = this
  override def unitDesignation: Symbol = eras.head.unitDesignation
  override def isDefinedAt(x: BigInt): Boolean = findEraByIndex(x).isDefinedAt(x)
  override def ticksOf(i: BigInt): BigInt = findEraByIndex(i).ticksOf(i)
  override def apply(i: BigInt) : Option[TimeUnit] = findEraByIndex(i).apply(i)
}

