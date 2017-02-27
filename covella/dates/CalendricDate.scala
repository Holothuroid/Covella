package org.covella.dates

import org.covella.Preamble.DatePath
import org.covella.dsl.{Calendar, InferedUnit, TimeUnit}

/**
  * This abstract class represents an internal representation for dates that acts as an intermediary between
  * human style Datum and computer style Timestamp. Its subclasses should not be created directly, but can
  * be created with Datum::check and Timestamp::interpret.
  *
  * Subclasses mark correctness and completeness of the date according to the calendar.
  */

 abstract class CalendricDate(val paths: Set[DatePath], val cal: Calendar) extends DateLike  {

  def get(unit: TimeUnit,context: TimeUnit = InferedUnit)(implicit cal: Calendar) : Option[BigInt] = ???

 def datum : Datum = (for (path <- paths) yield path.head.unit>path.head.index &
   (for ( i <- 1 until path.size) yield path(i).unit / path(i-1).unit > path(i).index ).reduce(_ & _) ).reduce(_&_)


  def begins : Option[Timestamp] = None
  def ends: Option[Timestamp] = None
  def timestamp : Option[Timestamp] = None

  val stringPrefix : String

  override def toString : String =  (for (path <- paths) yield { for( entry <- path) yield
    (entry.unit + ": " + entry.index + entry.marker)}.mkString(" - ") ).mkString(stringPrefix+"("," // ",")")

}


/**
  * Dates with this type can happen in the given Calendar.
  */

abstract class PossibleDate(paths: Set[DatePath], cal: Calendar) extends CalendricDate(paths,cal) {
  require( ( for(path<-paths) yield path.map(_.marker).filter(_ != OK ) ).flatten.isEmpty  )

}


/**
  * Dates with this type, have some defined beginning and end.
  */
abstract class DefiniteDate(paths: Set[DatePath], cal: Calendar) extends PossibleDate(paths,cal){

  override def begins = ???
  override def ends = ???

}

/**
  * Dates with this type, have defined beginning and end and describe a space that is longer than one tick.
 */

case class TimeframeDate(override val paths: Set[DatePath],override val  cal: Calendar) extends DefiniteDate(paths,cal){
  val stringPrefix = "TimeframeDate"
}

/**
  * Describes a date pointing to a single tick.
  */

case class PointDate(override val paths: Set[DatePath],override val  cal: Calendar) extends DefiniteDate(paths,cal){
  override def timestamp = begins
  val stringPrefix = "PointDate"
}

/**
  * Describes a possible whose DatePaths do not all start with the Calendar's Era.
  * They are possible but cannot be assigned to certain time frame.
  */

case class DanglingDate(override val paths: Set[DatePath], override val cal: Calendar) extends PossibleDate(paths,cal){
  val stringPrefix = "DanglingDate"
}

/**
  * This describes dates that do not conform the given Calendar, i.e. those that have DatePathMarkers other than OK.
  * Consult ::errors for a set of those markers.
  */


abstract class ErroneousDate(override val paths: Set[DatePath],override val  cal: Calendar) extends CalendricDate(paths,cal)

case class DisqualifiedDate(override val paths: Set[DatePath],override val  cal: Calendar) extends ErroneousDate(paths,cal){
  val errors : Set[DatePathMarker] = paths.flatMap(_.map(_.marker)).filter(_ != OK)
  require(! errors.isEmpty)

  val stringPrefix = "DisqualifiedDate"
}

/**
  * Describes a date with at least two paths that do not describe overlapping time frames.
  */

case class ContradictoryDate (override val paths: Set[DatePath],override val  cal: Calendar)  extends ErroneousDate(paths,cal) {
  require(paths.size>1)

  val stringPrefix = "ContradictoryDate"
}



/**
  * This trait holds entries that were disregarded on Datum::check, because they are not part of the Calendar.
  */

trait DisregardedUnits  extends  CalendricDate {

  val unknownUnits : Set[(TimeUnit,BigInt,TimeUnit)]

  override def toString = super.toString + "with DisregardedUnits(" + unknownUnits.mkString(", ") + ")"
}




