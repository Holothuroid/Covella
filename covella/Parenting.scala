package com.github.holothuroid.covella

/**
  * This trait describes TimeUnit that contain a certain `amount` of other measurables.
  * All children should have the same subunits but can vary in length.
  */

trait Parenting extends TimeUnit {
  def childDesignation : Symbol
  def amount: Int

  def withOffset (i: Int) = WithOffset(this,i)
  def excluding (ints: Int*) = Excluding(this,ints.map(BigInt(_)).toSet)
  def excludingAll(ints : Iterable[Int]) = Excluding(this,ints.map(BigInt(_)).toSet)

}


/**
  * Marker trait denoting that this parents counts its children continously beginning at 0.
  */

trait PureParent extends Parenting{
  def withNames (names: String * )  = WithNames(this,names.map(_.trim))
}


/**
  * Repeats another TimeUnit a certain number of times.
  * @param designation The name for this unit as a Symbol.
  * @param unit The repeated unit.
  * @param amount How often?
  */


case class Measure(designation: Symbol, unit: TimeUnit, amount: Int ) extends PureParent {
  require(! unit.subunits.contains(designation) )
  lazy val ticks = amount * unit.ticks
  lazy val subunits = designation +: unit.subunits
  def childDesignation = unit.designation

  def check(datum: Datum) : Datum =
      unit.check(datum.update(childDesignation,_.requireValue(0,amount)))


  def byTicks(tocks: BigInt) : Either[String,Datum] = {
    tocks match { case x if x >= 0 && x < ticks => unit.byTicks(x% unit.ticks) map (_ & new Datum(unit.designation->Ok(x/unit.ticks)) )                                                                   // unit.byTicks(x% unit.ticks) )
    case _ => Left(s"Something went wrong in Measure $designation. Got: $tocks Have: $ticks") }

  }

  def timestamp(datum: Datum) =
    for(index <- datum get unit.designation  ;
        subsequentTicks <- unit.timestamp(datum))  //;
      yield unit.ticks *index + subsequentTicks


  def timestampOrZero(datum: Datum) : BigInt =
    unit.ticks * datum.get(unit.designation).getOrElse(0) + unit.timestampOrZero(datum)

  override private[covella] def amountInImpl(tu: Symbol) : Int = amount * unit.amountIn(tu)


}

/**
  * A more complex Parenting unit. Children don't have to be the same.
  * @param designation This unit's designation.
  * @param children A Seq of TimeUnit. They are required to have the same subunits.
  */

case class Cycle(designation: Symbol, children: Seq[TimeUnit]) extends PureParent {
  require(children.forall(_.subunits == children.head.subunits))

  def childDesignation = children.head.designation

  def amount = children.size

  lazy val ticks = childTicks.sum

  override private[covella] def amountInImpl(tu: Symbol) = children map (_.amountIn(tu)) sum

  def subunits = designation +: children.head.subunits

  lazy val childTicks: Seq[BigInt] = children.map(_.ticks)
  lazy val ticksUntil: Seq[BigInt] = childTicks.scanLeft(BigInt(0))(_ + _)


  def check(datum: Datum) : Datum = {
    val newDatum = datum.update(childDesignation,_.requireValue(0,amount))

    newDatum.get(childDesignation) match {
      case None => newDatum.remove(subunits.tail)
      case Some(i) => children(i.toInt).check(newDatum)
    }
  }

  def byTicks(tocks: BigInt): Either[String, Datum] = {
    if (tocks < 0 || tocks >= ticks) return Left(s"Something went wrong in Cycle $designation. Got: $tocks. Have: $ticks")

    val fittingSegment: Option[(BigInt, Int)] = ticksUntil.zipWithIndex.takeWhile(_._1 <= tocks).lastOption

    fittingSegment match {
      case Some((lowerBound, index)) =>
        val remaining = tocks - lowerBound

        children(index).byTicks(remaining) map (_ & new Datum(childDesignation -> Ok(index)))
      case _ => Left(s"Something went wrong in Cycle $designation. Got: $tocks. Have: $ticks")
    }
  }

  def timestamp(datum: Datum): Option[BigInt] =
    for (index <- datum get childDesignation;
         subsequentTicks <- children(index.toInt).timestamp(datum))
      yield ticksUntil(index.toInt) + subsequentTicks

  def timestampOrZero(datum: Datum): BigInt = {
    val index: BigInt = datum.get(childDesignation).getOrElse(0)
    ticksUntil(index.toInt) + children(index.toInt).timestampOrZero(datum)
  }
}
