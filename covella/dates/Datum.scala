package org.covella.dates

import org.covella.Preamble.DatePath
import org.covella._
import org.covella.dsl._

/**
  * Created by 1of3 on 26.02.2017.
  */

/**
  * This class contains date with the units according to a Calendar.
  * They represent a human readable date and therefore named in good old Latin.
  * @param entries A set of triples, consisting of a indexed TimeUnit, the index (Int) and the unit the index is counted in.
  *                Therefore (day,2,week) represents the second day of a week.
  */


case class Datum(entries: Set[(TimeUnit,BigInt,TimeUnit)] ) extends DateLike with Calendable {

  require( entries.map(x => (x._1,x._3)).size == entries.size)  // Require that _2 is functionally dependent on _1 and _3.

  def keys = entries.map(_._1)
  def values = entries.map(_._2)
  def contexts = entries.map(_._3)


  override def get(unit: TimeUnit, context: TimeUnit = InferedUnit)(implicit cal: Calendar) = Option(apply(unit,context))

  private[covella] def apply(unit: TimeUnit, context: TimeUnit = InferedUnit) : BigInt  = {
    val result = entries.filter(entry => (entry._1 == unit && entry._3 == context))
    if (result.isEmpty) throw new IllegalArgumentException(s"Entry $unit/$context not contained in datum.")
    else result.head._2 }


  /**
    * This method tries to infer the standard contexts of entries that are marked as InferedUnit.
    * If a standard context cannot be found, the entry will remain unchanged.
    * @param cal The calendar according to which contexts shall be infered. May be implicit.
    * @return An updated copy of this datum.
    */

  def inferUnits(implicit cal: Calendar) : Datum =
  { val newEntries = for (entry <- entries) yield
    if (entry._3 == InferedUnit) (entry._1,entry._2,cal.standardContexts.getOrElse(entry._1,InferedUnit))
    else entry
    Datum(newEntries) }


  /**
    * Checks this datum according to a Calendar. Determines whether the datum is a PointDate, TimeframeDate,
    * DanglingDate or DisqualifiedDate (with certain DateError-s).
    * Entries that feature units not found in the calendar will result in DisregaredUnits.
    * @param cal The calendar in question. May be implicit.
    * @return Some subtype of CalendricDate.
    */

  def check(implicit cal: Calendar) : CalendricDate = {
    val entriesWithContexts = this.inferUnits(cal).entries

    val inCal : Set[(TimeUnit,BigInt,TimeUnit)] =
      entriesWithContexts.filter(x => cal.subunits.contains(x._1) && cal.subunits.contains(x._3))
    val unknownEntries : Set[(TimeUnit,BigInt,TimeUnit)] =
      entriesWithContexts.filter(x => !cal.subunits.contains(x._1) || !cal.subunits.contains(x._3))

    val contextEraUnit = inCal.filter(_._3 == cal.era.unit)
    var resultType = if (contextEraUnit isEmpty) 'dangling else 'definite // Das hier muss noch an eine andere Stelle oder abgefragt werden.

    var paths : Set[DatePath] = contextEraUnit.map(x => Seq(DatePathEntry(x._1,x._2,OK)))


    // Einzeln zuordnen.


    if(!  paths.flatMap(_.map(_.marker)).filter(_ != OK).isEmpty ) resultType = 'disqualified
        else if ( paths.flatMap(_.map(_.unit)).filter(_.isInstanceOf[Tick]).isEmpty  ) resultType = 'timeframe
              else resultType = 'point

    // Auf Contradiction prüfen

    if (unknownEntries.isEmpty) resultType match {
      case 'dangling          => DanglingDate(paths,cal)
      case 'timeframe         => TimeframeDate(paths,cal)
      case 'point             => PointDate(paths,cal)
      case 'contradiction     => ContradictoryDate(paths,cal)
      case _                  => DisqualifiedDate(paths,cal)   }

    else resultType match {
      case 'dangling    =>
        new DanglingDate(paths,cal) with DisregardedUnits {override val unknownUnits = unknownEntries }
      case 'timeframe   =>
        new TimeframeDate(paths,cal) with DisregardedUnits {override val unknownUnits = unknownEntries }
      case 'point       =>
        new PointDate(paths,cal) with DisregardedUnits {override val unknownUnits = unknownEntries }
      case 'contradiction     =>
        new ContradictoryDate(paths,cal) with DisregardedUnits {override val unknownUnits = unknownEntries }
      case _             =>
        new DisqualifiedDate(paths,cal) with DisregardedUnits {override val unknownUnits = unknownEntries }    }

  }








  def plus(that: Uniform)(implicit cal: Calendar): Option[Datum] = this.timestamp(cal).map(_.plus(that)).map(_.datum(cal))
  def + (that: Uniform)(implicit cal: Calendar): Option[Datum] = this.plus (that) (cal)
  def minus(that: Uniform)(implicit cal: Calendar): Option[Datum] = this.timestamp(cal).map(_.minus(that)).map(_.datum(cal))
  def - (that: Uniform)(implicit cal: Calendar): Option[Datum] = this.minus (that) (cal)
  def plus(that: CycledInstanceBuilder)(implicit cal: Calendar): Option[Datum] = this.timestamp(cal).map(_.plus(that)).map(_.datum(cal))
  def + (that: CycledInstanceBuilder)(implicit cal: Calendar): Option[Datum] = this.plus (that) (cal)
  def minus(that: CycledInstanceBuilder)(implicit cal: Calendar): Option[Datum] = this.timestamp(cal).map(_.minus(that)).map(_.datum(cal))
  def - (that: CycledInstanceBuilder)(implicit cal: Calendar): Option[Datum] = this.minus (that) (cal)
  def begins(implicit cal: Calendar): Option[Timestamp] = check(cal).begins
  def ends(implicit cal: Calendar): Option[Timestamp] = check(cal).ends
  def timestamp(implicit cal: Calendar): Option[Timestamp] = check(cal).timestamp



  // def forth(unit: TimeUnit,int: Int) : Datum = ???
  //def back(unit: TimeUnit,int: Int) : Datum = ???



  /**
    * Combines the information in two Datum objects. If there are formal discrepancies,
    * like the first Datum declaring month->1 and the second Datum declaring month->2, the second will take precendence.
    * The method does NOT check, whether the resulting Datum actually exists according to some calendar.
    * @param that Another Datum
    * @return A Datum with combined information.
    */

  def & (that: Datum) = Datum(this.entries ++ that.entries)



  /**
    * Combines the information in two Datum objects, after checking formal discrepancies.
    * A formal discrepancy arises, when one Datum declares a certain value for a TimeUnit and the other declaring a different value for the same unit.
    * The method does NOT check, whether the resulting Datum actually exists according to some calendar.
    * @param that Another Datum
    * @return A Datum with combined information.
    */

  def &? (that: Datum) :  Either[String, Datum] = { val sharedKeys = this.entries.map(entry => (entry._1,entry._3)) & entries.map(entry => (entry._1,entry._3))
    val discrepancies= for (key<-sharedKeys; if this.apply(key._1,key._2) != that.apply(key._1,key._2))
      yield key + ": " + this.apply(key._1,key._2) + " vs. " + that.apply(key._1,key._2)

    if(discrepancies.nonEmpty)  Left(discrepancies.mkString("Discrepancies in Data: \n","\n","")) else Right(this & that)
  }

  override def toString: String =
    entries.map { case (x,y,InferedUnit) => x + ": " + y
    case (x,y,z) => x + " of " + z + ": " + y }.mkString("Datum(",", ",")")

}

object Datum {

  //def apply(tuples: (TimeUnit,Int,TimeUnit)*) : Datum = Datum ( tuples.toSet  )
  def apply(seq: Seq[(TimeUnit,BigInt,TimeUnit)]) : Datum = Datum( seq.toSet )

}

