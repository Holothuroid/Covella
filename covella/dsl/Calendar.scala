/**
  * Created by 1of3 on 31.10.2016.
  */


package org.covella.dsl

import org.covella.dates.{Calendable, DateLike, ZeroDatum}

import scala.collection.mutable.HashMap

object Calendar{
  def apply(tui: TimeUnitInstance)  : Calendar = Calendar(Era rest tui)
}


/**
  * The calendar is the central part of the Covella DSL. Once you set up your Calendar you're good to go.
  * @param era First define units as you like, then put them into an Era and the Era into the Calendar class.
  */

case class Calendar (era: Era){


  /**
    * The standard contexts for each given time unit is used to interpret calls like ' day>7 '.
    * So if the standard context of days are months, ' day>7 ' will be treated as ' day of month>7 '
    * Standard contexts are assigned on construction of Calendar for the primary cycle and on synchronisation of Secondary Cycles.
    * In the current version the first match will always be used.
    * So if there are epagomenal instances unexpected things might happen.
    * Like the Armstrong and Aldrin Day in the Tranquility Calendar the standard context for days will be year not month.
    * Make sure you explicitly call ' day of month ' as appropriate, when you work with such a system.
    * The same method applies to secondary cycles.
    * In the Western Calendar days (as well as hours, minutes...) are found in the primary cycle under months and in a secondary cycle under week.
    * In such cases it is also 'first come, first served'. So the primary cycle's months win.
    * That also means that the order of secondary cycles can be relevant.
    *
    * When in doubt use explicite contexts.
    *
    * Depending on feedback another approach might be used in future versions, like having context interference be opt-in.
    */

  val standardContexts : HashMap[TimeUnit,TimeUnit] = HashMap()
  updateStandardContexts(era)

  def updateStandardContexts(parent: Parenting) {
    for (unit <- parent.childUnits; if !(standardContexts.keySet contains unit) ) standardContexts update(unit, parent.unit)

     parent match {
       case it : Era =>
         for (value <- it.values; if value.isInstanceOf[Parenting]) updateStandardContexts(value.asInstanceOf[Parenting])

       case it : CompositeInstance =>
         for(child <- it.children; if child.isInstanceOf[Parenting]) updateStandardContexts(child.asInstanceOf[Parenting])

       case it: CycledInstance => {val child = it.childUnit.newInstance()
                                if(child.isInstanceOf[Parenting]) updateStandardContexts(child.asInstanceOf[Parenting])}

       case _ =>
     }
  }


  def subunits = era.subunits ++ secondaryCycles.map(_.parent.subunits).reduce(_ union _)


  /**
    * A map of DateFormats for use with this calencar.
    */

  val dateFormats : HashMap[String,DateFormat] = HashMap()
  def addFormat(tuple: (String,DateFormat)) = {dateFormats += tuple; this}

  def format(dateLike: DateLike, formatName: String) = dateFormats(formatName).makeString(dateLike)
  def parser = dateFormats.values.map(_.fromString).reduceLeft(_ orElse _)
  def parseDate(string: String) = parser(string)

/*  private var timeStampRatio_ : (TimeUnit,Double) = ??? // smallest named unit : 1 Tick
  def timeStampRatio : String = "1 " + timeStampRatio_._1.designation + " = " + timeStampRatio_._2 + " tick(s)"
  def setTimeStampRatio(unit: TimeUnit,double: Double) = {timeStampRatio_ = (unit,double); this} */

  var timeStampZero : Calendable = ZeroDatum
  def setTimeStampZero(datum: Calendable) : this.type = {timeStampZero = datum; this}


  /**
    * Secondary cycles can complement the Calendar's primary cycle with weeks, moon phases and similar units.
    * @param parent A Parenting unit holding the secondary cycles's subunits.
    * @param atDate The date, according to the primary cycle, when synchronisation should happen. By default timeStampZero.
    * @param childseq The indices of the secondary cycles subunits at the point of synchronisation. By default the first possible point of parent.
    */

  case class SecondaryCycle(parent: Parenting, atDate: DateLike, childseq: BigInt*)
  val secondaryCycles : scala.collection.mutable.Seq[SecondaryCycle] = scala.collection.mutable.Seq()

  /**
    * Allows for definining the atDate of secondary cycle. Follow with 'synchronize'.
    */
  def at(atDate: DateLike) = SynchronisationBuilder(atDate,this)

  /**
    * Creates a secondary cycle and adds it to the Calendar
    * @param parent The Parenting unit of the SecondaryCycle.
    * @param childSeq The sequence of indices valid at point of synchronisation, by default, the first possible point of parent.
    * @return Returns the Calendar itself.
    */

  def synchronize(parent: Parenting, childSeq: BigInt*) : this.type  = {
    require(!(standardContexts contains parent.unit))

    val secCycle = if (!childSeq.isEmpty) SecondaryCycle(parent, timeStampZero, childSeq: _*)
                      else if (parent.isDefinedAt(0)) SecondaryCycle(parent, timeStampZero, 0)
                      else SecondaryCycle(parent, timeStampZero, 1)

    if (era.childUnits.size==1) standardContexts update (parent.unit, era.childUnits.head)
    else standardContexts update (parent.unit,era.unit)
    updateStandardContexts(parent)

    secondaryCycles :+ secCycle
    this
  }

}


case class SecondaryCycle(parent: Parenting, atDate: DateLike, childseq: BigInt*)

case class SynchronisationBuilder(atDate: DateLike, cal: Calendar) {

  def synchronize(parent: Parenting, childSeq: BigInt*): Calendar = {
    require(!(cal.standardContexts contains parent.unit))

    val secCycle = if (!childSeq.isEmpty) SecondaryCycle(parent, atDate, childSeq: _*)
    else if (parent.isDefinedAt(0)) SecondaryCycle(parent, atDate, 0)
    else SecondaryCycle(parent, atDate, 1)

    if (cal.era.childUnits.size==1) cal.standardContexts update (parent.unit, cal.era.childUnits.head)
                         else cal.standardContexts update (parent.unit,cal.era.unit)
    cal.updateStandardContexts(parent)
    cal.secondaryCycles :+ secCycle

    cal
  }
}

