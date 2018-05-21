package com.github.holothuroid.covella

/**
  * TimeUnits are the basic buidling blocks of a Calendar.
  * To build your Calendar create with a hierarchy of TimeUnits, starting with a Tick, then wrap it into Era and Calendar.
  * TimeUnits implement the DateHandler trait, allowing them to convert between human and computer dates.
  */

trait TimeUnit extends  DateHandler {
  /**
    * @return This unit's internal name, like 'year, 'month, 'stardate, 'moonphase
    */

  def designation : Symbol

  /**
    * @return The length of the unit measured in Ticks on the clock.
    */
  def ticks: BigInt

  /**
    * Used in template method `.amountIn`
    * @param tu The designation of a unit.
    * @return Should call `.amountIn` for this unit's subunits and sum.
    */

  private[covella] def amountInImpl(tu: Symbol) : Int

  /**
    * Returns how often some time unit fits into this unit.
    * @param tu The designation of a unit.
    * @return 1, if the symbol is this unit's own designation. 0, if the symbol is not among this unit's subunits.
    *         Otherwise call to .amountInImpl, which should call upon this unit's subunits and sum.
    */

  def amountIn(tu: Symbol) : Int = tu match {
    case i if i == this.designation => 1
    case x if ! (subunits contains x) => 0
    case y => amountInImpl(y)
  }

  /**
    * Creates a vector repeating this TimeUnit. Useful for creating Cycles.
    * @param i How often?
    * @return A Vector of TimeUnit.
    */
  def * (i: Int) : Vector[TimeUnit] = (1 to i).map(_ => this).toVector
}


/**
  * The basic unit in a Calendar with exactly one tick.
  * @param designation The name for this unit as a Symbol.
  */


case class Tick(designation: Symbol) extends TimeUnit{
  val ticks = 1
  val subunits = List(designation)

  def check(datum: Datum) = datum
  def byTicks(tocks: BigInt) = if (tocks==0) Right(Datum()) else Left("I'm a tick! You gave me:"+tocks)
  def timestamp(datum: Datum) : Option[BigInt] = if (datum.isOkAt(designation)) Some(0) else None
  def timestampOrZero(datum: Datum) : BigInt = 0

  override private[covella] def amountInImpl(tu: Symbol) = throw new IllegalAccessException("Attempted `amountInImpl` in class Tick. That should never happen.")
}




