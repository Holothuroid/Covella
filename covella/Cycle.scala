package com.githup.holothuroid.covella
import scala.language.implicitConversions

/**
  * A Cycle can be used to handle irregular time units like months in a year.
  * For simplicity the year and months are created together.
  * As is usual, the unit indices by standard count from 1! (This is different from Measures.)
  * @param designation A designation for the cycle, e.g. 'year
  * @param cyclingUnit An IrregularUnit, e.g. IrregularUnit('month).
  * @param measurableUnit A Measurable, the cyclingUnit is made of, e.g. days.
  * @param children A sequence of CycleChild instances, describing the appearance of the cyclingUnit.
  */

case class Cycle(designation: Symbol,
                 cyclingUnit: IrregularUnit,
                 measurableUnit: Measurable,
                 children: Seq[CycleChild]
                ) extends DateHandler with Measurable {
  require(!measurableUnit.subunits.contains(designation))
  require(!measurableUnit.subunits.contains(cyclingUnit.designation))
  require(this.designation != cyclingUnit.designation)

  lazy val ticks = measurableUnit.ticks * children.map(_.length).sum

  lazy val subunits =  designation :: cyclingUnit.designation :: measurableUnit.subunits

  lazy val childTicks : Seq[BigInt] = children.map(_.length * measurableUnit.ticks)
  lazy val ticksUntil : Seq[BigInt] = childTicks.scanLeft(BigInt(0))(_+_)



  def check(seq: Seq[String]): Datum = {
    if (seq.isEmpty) return Datum()
    val head = headEntry(seq, upper = children.length, lower = 1)
    val headDate = new Datum(cyclingUnit.designation -> head)

    if (seq.tail.isEmpty) return headDate

    val headAndNextDate = head match {
      case Ok(i, _) => headDate &
        new Datum(measurableUnit.designation ->
          headEntry(seq.tail, upper = children(i.toInt - 1).length, lower = 1))

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
            val indexMeasurable = remainingInCyclingUnit/measurableUnit.ticks+1

            measurableUnit.byTicks(remainingInMeasurable) map (_ & new Datum(cyclingUnit.designation -> Ok(index+1,name),
                                                                              measurableUnit.designation->Ok(indexMeasurable)))

      case _ =>  Left(s"Something went wrong in Cycle $designation. Got: $tocks. Have: $ticks")
    }
  }

  def timestamp(datum: Datum): Option[BigInt] =
      for(indexCycling <- datum get cyclingUnit.designation  ;
          indexMeasurable <- datum get measurableUnit.designation ;
          subsequentTicks <- measurableUnit.timestamp(datum) )
        yield ticksUntil(indexCycling.toInt -1) + (indexMeasurable -1)*measurableUnit.ticks + subsequentTicks

  def timestampOrZero(datum: Datum): BigInt = {
     ticksUntil( datum.get(cyclingUnit.designation).map(_-1).getOrElse(BigInt(0)).toInt ) +
       datum.get(measurableUnit.designation).map(_-1).getOrElse(BigInt(0)) * measurableUnit.ticks +
        measurableUnit.timestampOrZero(datum)
  }

}


object CycleChild{
  implicit def tuple2Child(tuple: (Int,String)) : CycleChild = new CycleChild(BigInt(tuple._1),Some(tuple._2))
  implicit def int2Child(i : Int) : CycleChild = new CycleChild(BigInt(i))
  implicit def string2Child(name : String) : CycleChild = new CycleChild(BigInt(1),Some(name))
}

case class CycleChild (length: BigInt, name: Option[String] = None)

//case class Epagomenal(nam: String) extends CycleChild(1,Some(nam)){
//  val length = 1
//  val name = Some(nam)
//}