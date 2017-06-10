package com.githup.holothuroid.covella

/**
  * Created by 1of3 on 04.06.2017.
  */

import scala.language.implicitConversions

case class Cycle(designation: Symbol,
                 cyclingUnit: IrregularUnit,
                 measurableUnit: Measurable,
                 children: Seq[CycleChild] /*Refined NonEmpty*/
                ) extends DateHandler with Measurable {
  require(!measurableUnit.subunits.contains(designation))
  require(!measurableUnit.subunits.contains(cyclingUnit.designation))
  require(this.designation != cyclingUnit.designation)

  lazy val ticks = measurableUnit.ticks * children.map(_.length).sum
//  def isAlias = children.forall(_.length == 1)
  lazy val subunits =  designation :: cyclingUnit.designation :: measurableUnit.subunits

  lazy val childTicks : Seq[BigInt] = children.map(_.length * measurableUnit.ticks)
  lazy val ticksUntil : Seq[BigInt] = childTicks.scanLeft(BigInt(0))(_+_)

  //lazy val indices = children.map

  def check(seq: Seq[String]): Datum = {
    if (seq.isEmpty) return Datum()
    val head = headEntry(seq, upper = children.length, lower = 1)
    val headDate = new Datum(cyclingUnit.designation -> head)

    if (seq.tail.isEmpty) return headDate

    val headAndNextDate = head match {
      case Ok(i, _) => headDate &
        new Datum(measurableUnit.designation ->
          headEntry(seq.tail, upper = children(i.toInt - 1).length, lower = 1)) // this is dangerous

      case _ => headDate &
        new Datum(measurableUnit.designation -> Unknown(seq.tail.head.toString))
    }

    if (seq.tail.length == 1) headAndNextDate else headAndNextDate & measurableUnit.check(seq.tail.tail)
  }



  def byTicks(tocks: BigInt): Either[String, Datum] = {
    if (tocks < 0 || tocks >= ticks) return Left(s"Something went wrong in Cycle $designation. Got: $tocks. Have: $ticks")

    val fittingSegment : Option[(BigInt,Int)] = ticksUntil.zipWithIndex.takeWhile(_._1 <= tocks).lastOption

    fittingSegment match {
      case Some((lowerBound,index))  =>
            val remainingInCyclingUnit = tocks-lowerBound
            val name = children(index).name
            val remainingInMeasurable = remainingInCyclingUnit%measurableUnit.ticks
            val indexMeasurable = remainingInCyclingUnit/measurableUnit.ticks +1

            measurableUnit.byTicks(remainingInMeasurable) map (_ & new Datum(cyclingUnit.designation -> Ok(index+1,name),
                                                                              measurableUnit.designation->Ok(indexMeasurable)))

      case _ =>  Left(s"Something went wrong in Cycle $designation. Got: $tocks. Have: $ticks")
    }
  }

  def timestamp(datum: Datum): Option[BigInt] =
      for(indexCycling <- datum get cyclingUnit.designation  ;
          indexMeasurable <- datum get measurableUnit.designation ;
          subsequentTicks <- measurableUnit.timestamp(datum) )
        yield ticksUntil(indexCycling.toInt) + indexMeasurable*measurableUnit.ticks + subsequentTicks



}


object CycleChild{
  implicit def tuple2Child(tuple: (Int,String)) : CycleChild = CycleChild(BigInt(tuple._1),Some(tuple._2))
  implicit def int2Child(i : Int) : CycleChild = CycleChild(BigInt(i))
  implicit def string2Child(name : String) : CycleChild = CycleChild(BigInt(1),Some(name))
}

case class CycleChild (length: BigInt, name: Option[String] = None)
