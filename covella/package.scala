package com.github.holothuroid
package object covella {

  /**
    * Extension methods for turning strings into covella objects.
    * @param string THe enriched string
    */

  implicit class CovellaStringOps(val string: String) extends AnyVal{

    def dateInCalendar(implicit cal: Calendar) : Datum = cal.parse(string)(date)
    def timeInCalendar(implicit cal: Calendar) : Datum = cal.parse(string)(time)
    def datetimeInCalendar(implicit cal: Calendar) : Datum = cal.parse(string)(datetime)

  }

  /**
    * Extension methods for constructing Measurables.
    * @param enrichedSymbol The symbol for the Measurable to be constructed.
    */

  implicit class UnitFactory(val enrichedSymbol: Symbol) extends AnyVal{

    def of(tuple: (TimeUnit,Int)) : Measure = Measure(enrichedSymbol,tuple._1,tuple._2)
    def of(measurable: TimeUnit) =
      new {
        def withNames(names: String*) = WithNames(Measure(enrichedSymbol,measurable,names.size),names)
      }

    def isAliasFor(measurable: TimeUnit) = TimeUnitAlias(enrichedSymbol,measurable)

    def isCycleOf(children : TimeUnit*) : Cycle = Cycle(enrichedSymbol, children)
    def cycles(namedChildren : (TimeUnit,String)*) : WithNames =
      Cycle(enrichedSymbol, namedChildren.map(_._1)) withNames (namedChildren.map(_._2) :_*)

    def cycles (childUnit: TimeUnit) =
          new { def as(childSymbol: Symbol) =
                  new {
                        def comprising(ints: Int* ): Cycle = {
                          val children = ints map (i => Measure(childSymbol,childUnit,i))
                          Cycle(enrichedSymbol,children)
                        }


                        def comprising(tuples: (Int,String)*) : WithNames =
                          comprising(tuples.map(_._1) : _*).withNames(tuples.map(_._2) : _*)

                  }
          }
  }



  implicit class DateFormatFactory(val sc: StringContext) extends  AnyVal{

    def df(args: DateFormat*) : DateFormat = {

      val parsers = args.map(_.parse )
      val getters = args.map(_.format)
      val placeholders = args.map(_.placeholder)

      import scala.util.matching.Regex
      // Interpolate the placeholders, then turn into a regex.
      val interpolated = sc.parts.zipAll(placeholders,"","").map{case (x,y) => x+y}.mkString("")
      val regex : Regex = interpolated.r

      val parse_ : PartialFunction[String,Datum] = {   // Create a function String=>Datum that pattern matches on the regex.
        case regex(strings@_*)  => parsers zip strings map { case (function, arg) => function(arg) } reduce (_&_)
        case s                  => new Datum('dateFormatDidNotMatch -> Unknown(s))
      }

      // For the .format evaluate the datum for each getter, then use the StringContext we started the factory with.
      // Using .s on the StringContext will interpolate the strings we got.
      val format_ = (datum: Datum) => { val applieds = getters.map(_ apply datum) ;
                                            sc.s(applieds:_*) }

      // For a new placeholder, remove capture groups, then add new parentheses
      val placeholder_ = "(" + interpolated.filter(c => c!='(' && c!=')') + ")"

      DateFormat(parse = parse_ , format = format_ , placeholder_ )
    }
  }


  def num(symbol: Symbol, padding : Int = 2) : DateFormat ={
    val parse_  : String=>Datum = (string: String)=> {
      import scala.util.{Failure, Success, Try}
      Try(string.toInt) match {
        case Success(i) => Datum.of(symbol-> i)
        case Failure(e) => new Datum(symbol -> Unknown(string))
      }
    }

    val format_ : Datum=>String = (datum: Datum) =>
      datum.get(symbol).map(s"%0${padding}d".format(_)).getOrElse("#"*padding)

    DateFormat(format=format_ , parse =parse_, placeholder = s"(\\d{$padding,$padding})" )
  }

  def nam(symbol: Symbol, default : String = "UNKNOWN", placeholder : String = "(.*)") : DateFormat ={
    val parse_  : String=>Datum = (string: String)=> Datum.ofNames(symbol -> string.trim)
    val format_ : Datum=>String           = (datum: Datum) =>
      datum.getName(symbol).getOrElse(default)

    DateFormat(format=format_ , parse =parse_ , placeholder = placeholder )
  }

  lazy val y = num('year,4)
  lazy val Y = nam('year,"#YEAR")
  lazy val m = num('month)
  lazy val M = nam('month,"#MONTH")
  lazy val d = num('day)
  lazy val D = nam('day,"#DAY")
  lazy val h = num('hour)
  lazy val H = nam('hour,"#HOUR")
  lazy val min = num('minute)
  lazy val Min = nam('minute,"#MINUTE")
  lazy val s = num('second)
  lazy val S = nam('second,"#SECOND")

  lazy val date = df"$y-$m-$d"
  lazy val time = df"$h:$min:$s"
  lazy val datetime = df"$y-$m-$d $h:$min:$s"


  implicit class TimeStreamOps(val stream: Stream[Timestamp]) extends AnyVal{
    def matching(datum: Datum)(implicit cal: Calendar) : Stream[Timestamp] =
      stream.filter(_.inCalendar(cal).matches(datum))

    def before(t: Timestamp) : Stream[Timestamp] = stream.filter(_ < t)
    def before(d: Datum) : Stream[Timestamp] =
      stream.filter{
        d.begins match {
          case Some(t) => _ < t
          case None => _ => true // A datum without fixed beginning should not filter.
        }
      }

    def after(t: Timestamp) : Stream[Timestamp] = stream.filter(_ > t)
    def after(d: Datum) : Stream[Timestamp] =
      stream.filter{
        d.ends match {
          case Some(t) => _ > t
          case None => _ => true // A datum without fixed ending should not filter.
        }
      }

  }

  /**
    * Lifts a TimeUnit one or more levels to fit an Era's list of units.
    * Used in examples TranquilityCalendar and HarptosCalendar
    * @param tu A TimeUnit to be lifted.
    * @param name A name.
    * @param wrappers One or more Symbols. Start with largest unit going down.
    * @return (TimeUnit,String) The wrapped TimeUnit with its chosen name.
    */

  def epagomenal(tu: TimeUnit, name: String, wrappers : Symbol* ) : (TimeUnit,String) = {
    val allWrappedExceptForHead = wrappers.tail.foldRight(tu)((wrapper,t) => wrapper of (t,1) withNames name)
    (wrappers.head of (allWrappedExceptForHead,1),name)
  }


}
