package com.github.holothuroid.covella


/**
  * This trait includes utility methods for converting date information between different formats.
  * Known implementors: Measurable, Era
  */

trait DateHandler {
  def subunits: Seq[Symbol]

  /**
    * This method checks a datum, turning all `Unchecked...` DatumEntries to either Ok or a DatumError.
    * @param datum The Datum to be checked.
    * @return A new Datum with all entries checked.
    */

  def check(datum: Datum) : Datum

  /**
    * This method possibly turns a number into a Datum.
    * A DateHandler will typically find some entry for the Datum then pass on what remains.
    * @param tocks The total number.
    * @return Either an error String or Datum.
    */
  def byTicks(tocks: BigInt) : Either[String,Datum]

  /**
    * Measures how long it takes to reach a Datum.
    * Handlers should delegate to the correct subhandler and add how long it takes to reach that subhandler
    * @param datum A Datum.
    * @return Maybe a big number, representing an instant on the time line.
    *         For this method to return some number, the datum should be complete and without error (cf. Datum).
    */

  def timestamp(datum: Datum)   : Option[BigInt]

  /**
    * Similar to `.timestamp`, but missing date entries will be assumed being 0.
    * @param datum A Datum.
    * @return A big number, representing the beginning of the given Datum on the time line.
    */

  def timestampOrZero(datum: Datum)   : BigInt



}







