package com.githup.holothuroid.covella

/**
  * Created by 1of3 on 04.06.2017.
  */



trait TimeUnit extends Any{
  def designation : Symbol
}

case class IrregularUnit(designation: Symbol) extends  AnyVal with TimeUnit



/**
  * This trait includes utility methods for converting date information between different formats.
  * Known implementors: Measurable, Era
  */

trait DateHandler {
  def subunits: List[Symbol]

  /**
    * This method creates a datum from stringly input.
    * Generally the current DateHandler will handle part of the input and call on its first subunit to carry on.
    * @param seq The input is expected to be of type Seq[String] where each handler in the series treats one segment.
    * @return A Datum.
    */

  def check(seq : Seq[String]) : Datum

  /**
    * This method possibly turns a number into a Datum.
    * A DateHandler will typically find some entry for the Datum then pass on what remains.
    * @param tocks The total number.
    * @return Either an error String or Datum.
    */
  def byTicks(tocks: BigInt) : Either[String,Datum]

  /**
    * This method maybe turns a Datum into a number.
    * Usually a DateHandler will measure how long it takes to reach the unit it handles, then requests information from its subunits.
    * For this method to return some number, the datum should be complete and without error (cf. Datum).
    * @param datum A Datum.
    * @return Maybe Datum.
    */

  def timestamp(datum: Datum)   : Option[BigInt]
  def timestampOrZero(datum: Datum)   : BigInt


  private lazy val pattern = """(\d+)""".r

  protected def headThing(seq : Seq[String]) : Either[String,Int] = {
    seq.head match { case pattern(x) => Right(x.toInt)
    case x: String => Left(x) }
  }
  protected def headEntry(seq : Seq[String],upper: BigInt,lower: BigInt) : DatumEntry = {
    headThing(seq) match { case Right(i) if i>upper => TooHigh(i,upper)
    case Right(i) if i<lower => TooLow(i,lower)
    case Right(i) => Ok(i)
    case Left(x) => Unknown(x)}
  }

}







