package org.covella

import org.covella.dates.{DateLike, DatePathEntry, Datum, Timestamp}
import org.covella.dsl._

/**
  * Created by 1of3 on 01.11.2016.
  */
 object Preamble{


  /**
    * This implicit class extends sequences of TimeUnitInstances with the overloaded operator '+', as well as 'as'.
    * @param seq Some mutable or immutable sequence of TimeUnitInstances.
    */

  implicit class TimeUnitInstanceTransformer(val seq : collection.Seq[TimeUnitInstance]) extends AnyVal{

    /**
      * Turn a sequence of TimeUnitInstances into a CompositeInstance.
      * @param unit An Irregular TimeUnit.
      * @return A CompositeInstance.
      */
    def as(unit: IrregularUnit) = CompositeInstance(unit, children =seq)

     def + (that: TimeUnitInstance) : Seq[TimeUnitInstance] = seq :+ that
     def + (that: CycledInstanceBuilder) : Seq[TimeUnitInstance] = seq ++ that.unpacked
     def + (that: Seq[TimeUnitInstance]) : Seq[TimeUnitInstance] = seq ++ that
  }

  /**
    * This implicit class extends strings with conversion methods String#toDatum and String#toTimestamp.
    * Both methods take an (implicit) calendar as parameter.
    */

  implicit class DateParserExtension(val string: String) extends AnyVal {
    def toDatum(implicit cal: Calendar)     : Datum             = cal.parseDate(string) // Sollte das nicht auch Option sein?
    def toTimestamp(implicit cal: Calendar) : Option[Timestamp] = string.toDatum(cal).timestamp(cal)
  }


  def num(designator: TimeUnit)(minLength: Int = 1) = { // Hier fehlen contexts!!!!!
    def to(datum: DateLike) = s"%0${minLength}d" format( datum.get(designator).getOrElse(-1) )
    def from(string: String)   = designator -> string.toInt

    DateFormatHelper(to, from)
  }

  def nam(designator: TimeUnit)(alias: String = "default") = {
    def to(datum: DateLike) = datum.getAlias(designator,alias)
    def from = ??? // getName is not implemented yet, nor its reversal

    DateFormatHelper(to, from)

  }

  /**
    * This implicit class extends BigInts for easy divisilibity checks that are common in intercalation rules.
    * (cf. Era#given)
    */

  implicit class DivisibilityCheckerOps (val thisInt : BigInt) extends AnyVal {
  def divisibleBy(thatInt: BigInt)  = thisInt % thatInt == 0
  def notDivisibleBy(thatInt: BigInt)  = thisInt % thatInt != 0
  }

  /**
    * This implicit class provides the StringContext df"..." to create new date formats.
    * Interpolated parameters should be DateFormatHelpers.
    */

  implicit class DateFormatFactory(val sc: StringContext) extends  AnyVal{

    def df(args: Any*) : DateFormat = {

        if (args.map(_.isInstanceOf[DateFormatHelper]).reduce(_ && _))   {

          val formatHelpers = args.map(_.asInstanceOf[DateFormatHelper])
          val parsers = formatHelpers.map(_.from)
          val getters = for (getter<- formatHelpers.map(_.to)  ) yield (date: DateLike) => getter(date)

          val placeholders: collection.Seq[String] = for (i <- args.indices) yield "(.*)"
          val regex = sc.parts.zipAll(placeholders, "", "").map { case (a, b) => a + b }.mkString("").r

          val from: PartialFunction[String, Datum] = {
            case regex(strings@_*)
            => val entries = parsers zip strings map { case (function, arg) => function(arg) }
              val tuples = for ( (x,y) <- entries ) yield (x,BigInt(y),InferedUnit)
              Datum(tuples)
          }

          val make = (datum: DateLike) => { val applieds = getters.map(_ apply datum) ;  sc.s(applieds:_*) }


          DateFormat(make , from) }


        else throw new IllegalArgumentException("Unknown calls in DateFormat.")
      }
    }


  type DatePath = Seq[DatePathEntry]

  implicit var calendar : Calendar = _

}