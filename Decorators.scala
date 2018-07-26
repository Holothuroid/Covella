package com.github.holothuroid.covella


/**
  * This trait abstracts over decorators for time units.
  * The abstract methods .decorate and .undecorate manipulate a Datum and are automatically applied to the methods inherited from DateHandler.
  * @tparam T A subtype of DateHandler.
  */

trait Decorator [T <: DateHandler]{
  val decoratee: T
  def subunits : Seq[Symbol] = decoratee.subunits
  def decorate(datum: Datum) : Datum
  def undecorate(datum: Datum) : Datum

  // Methods going up, towards a human user, get decorated.
  def byTicks(tocks: BigInt): Either[String, Datum] =  decoratee.byTicks(tocks).map(decorate)

  // Methods going down, towards computer dates, get undecorated.
  def timestamp(datum: Datum): Option[BigInt] = decoratee.timestamp( undecorate(datum) )
  def timestampOrZero(datum: Datum) : BigInt = decoratee.timestampOrZero( undecorate(datum) )

  // Methods going both ways...
  def check(datum: Datum): Datum = decorate( decoratee.check(undecorate( datum) ) )
}



trait TimeUnitDecorator[T <: TimeUnit] extends Decorator[T] with TimeUnit{
  def ticks : BigInt = decoratee.ticks
  def designation : Symbol = decoratee.designation
  override private[covella] def amountInImpl(tu: Symbol) = decoratee.amountIn(tu)

  override def delegate(datum: Datum): Option[TimeUnit] = Some(decoratee)
}

/**
  * This class wraps a TimeUnit and renames its designation.
  * It can be used in subsequent calendars in a CalendarSystem to expose certain units that would be shadowed by previous entries.
  *
  * @param designation The new designation for the contained unit.
  * @param decoratee The unit whose designation should be changed.
  */


case class TimeUnitAlias(override val designation: Symbol, decoratee: TimeUnit) extends TimeUnitDecorator[TimeUnit]{
  override def subunits: Seq[Symbol] = designation +: decoratee.subunits.tail
  val unitDesignation : Symbol = decoratee.designation

  override def undecorate(datum: Datum) : Datum = {
    val newEntries = datum.entries.map
    { case (`designation`,value) => (unitDesignation,value)
    case foo => foo}
    datum.copy(entries = newEntries)
  }

  override def decorate(datum: Datum) : Datum = {
    val newEntries = datum.entries.map
    { case (`unitDesignation`,value) => (designation,value)
    case foo => foo}
    datum.copy(entries = newEntries)
  }

  override def amountInImpl(tu: Symbol): Int = tu match {
    case x if x == this.designation => decoratee.amountIn(decoratee.designation)
    case _ => decoratee.amountIn(tu)
  }

}


trait ParentingDecorator extends TimeUnitDecorator[Parenting] with Parenting{
   override val childDesignation = decoratee.childDesignation
   override val amount = decoratee.amount
}

case class WithOffset(decoratee: Parenting, offset: Int) extends ParentingDecorator{

  override def decorate(datum: Datum) : Datum = {
    val newEntries = datum.entries.map {
      case (`childDesignation`,Ok(number,maybeName)) => (childDesignation,Ok(number+offset,maybeName))
      case (`childDesignation`,TooHigh(number,bound)) => (childDesignation,TooHigh(number+offset,bound+offset))
      case (`childDesignation`,TooLow(number,bound)) => (childDesignation,TooLow(number+offset,bound+offset))
      case foo => foo
    }

    datum.copy(entries = newEntries)
  }

  override def undecorate(datum: Datum) : Datum = {
    val newEntries = datum.entries.map {
      case (`childDesignation`,Ok(number,maybeName)) => (childDesignation,Ok(number-offset,maybeName))
      case (`childDesignation`,UncheckedIndex(number)) => (childDesignation,UncheckedIndex(number-offset))
      case (`childDesignation`,UncheckedPair(number,tentativeName)) => (childDesignation,UncheckedPair(number-offset,tentativeName))
      case (`childDesignation`,TooHigh(number,bound)) => (childDesignation,TooHigh(number-offset,bound-offset))
      case (`childDesignation`,TooLow(number,bound)) => (childDesignation,TooLow(number-offset,bound-offset))
      case foo => foo
    }

    datum.copy(entries = newEntries)
  }


}


case class Excluding(decoratee: Parenting, excluded : Set[BigInt]) extends ParentingDecorator {

  private def jumps(i: BigInt) = excluded.count(_ <= i)

  override def decorate(datum: Datum) : Datum = {
    val newEntries = datum.entries.map {
      case (`childDesignation`,Ok(number,maybeName)) => (childDesignation,Ok(number+jumps(number),maybeName))
      case (`childDesignation`,TooHigh(number,bound)) => (childDesignation,TooHigh(number+jumps(number),bound+jumps(number)))
      case (`childDesignation`,TooLow(number,bound)) => (childDesignation,TooLow(number+jumps(number),bound+jumps(number)))
      case foo => foo
    }

    datum.copy(entries = newEntries)
  }

  override def undecorate(datum: Datum) : Datum = {

    val newEntries = datum.entries.map {
      case (`childDesignation`,Ok(number,maybeName)) => (childDesignation,Ok(number-jumps(number),maybeName))
      case (`childDesignation`,UncheckedIndex(number)) if excluded.contains(number) => (childDesignation,Forbidden(number))
      case (`childDesignation`,UncheckedPair(number,tentativeName)) if excluded.contains(number) => (childDesignation,Forbidden(number))
      case (`childDesignation`,UncheckedIndex(number)) => (childDesignation,UncheckedIndex(number-jumps(number)))
      case (`childDesignation`,UncheckedPair(number,tentativeName)) => (childDesignation,UncheckedPair(number-jumps(number),tentativeName))
      case (`childDesignation`,TooHigh(number,bound)) => (childDesignation,TooHigh(number-jumps(number),bound-jumps(number)))
      case (`childDesignation`,TooLow(number,bound)) => (childDesignation,TooLow(number-jumps(number),bound-jumps(number)))
      case foo => foo
    }

    datum.copy(entries = newEntries)
  }


}

trait PureParentDecorator extends TimeUnitDecorator[PureParent] with Parenting{
  override val childDesignation = decoratee.childDesignation
  override val amount = decoratee.amount
}


case class WithNames(decoratee: PureParent, names : Seq[String]) extends PureParentDecorator {
  require(names.size == amount)
  require(names.map(_.trim) == names)
  require(names.map(_.trim).distinct.size == names.size)

  override def decorate(datum: Datum): Datum = datum.update(childDesignation, _ match {
    case Ok(i,_) => Ok(i,Option(names(i.toInt)))
    case x => x
    }
  )

  override def undecorate(datum: Datum): Datum = {
    datum(childDesignation) match {
      case Some(UncheckedName(tentativeName)) => decoratee.check(
        datum.update(designation, UncheckedIndex(names.indexOf(tentativeName.trim))
        )
      )
      case Some(UncheckedPair(tentativeIndex,tentativeName)) =>
        if ( names(tentativeIndex.toInt) == tentativeName.trim )
          decoratee.check(
            datum.update(designation, UncheckedIndex(tentativeIndex) )
          )
        else datum.update(designation,Incongruent(tentativeIndex,tentativeName)).remove(subunits.tail)

      case _ => datum
    }
  }
}


