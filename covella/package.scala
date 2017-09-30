package com.githup.holothuroid

/**
  * Created by 1of3 on 04.06.2017.
  */
package object covella {

  implicit class CovellaStringOps(val string: String) extends AnyVal{

    def inCalendar(implicit cal: Calendar) = cal.check(string)
   // def withDateFormat(implicit cal: Calendar) = cal.parse(string)

  }

  implicit class UnitFactory(val symbol: Symbol) extends AnyVal{

    def is(tuple: (Measurable,Int)) : Measure = Measure(symbol,tuple._1,tuple._2)

    def isAliasFor(measurable: Measurable) = TimeUnitAlias(symbol,measurable)

    def isCycleOf (childDesg_ : Symbol) =
      new { val desg=symbol
            val childDesg = childDesg_
            def madeFrom(measurable_ : Measurable) =
              new {
                val desg=symbol
                val childDesg = childDesg_
                val measurable = measurable_

                def comprising(children: CycleChild*) =
                  OldenCycleCycle(desg,IrregularUnit(childDesg),measurable,children)

              }
      }
  }



  implicit class DateFormatFactory(val sc: StringContext) extends  AnyVal{

    def df(args: DateFormatHelper*) : DateFormat = {

        val parsers = args.map(_.parse )
        val getters = args.map(_.format)
        val placeholders = args.map(_.placeholder)

        val regex = sc.s(placeholders).r // Interpolate the placeholders using the s"..." function, then turn into a regex.

        val parse_ : PartialFunction[String, Seq[(Symbol,String)]] = {   // Create a function String=>Datum that pattern matches on the regex.
          case regex(strings@_*)  => parsers zip strings map { case (function, arg) => function(arg) }

          case s                  => Seq(('dateFormat_Did_Not_Match,s))
        }


        // For the .format evaluate the datum for each getter, then use the StringContext we started the factory with.
        // Using .s on the StringContext will interpolate the strings we got.
        val format_ = (datum: Datum) => { val applieds = getters.map(_ apply datum) ;
                                            sc.s(applieds:_*) }


        DateFormat(parse = parse_ , format = format_ )
    }
  }


  def num(symbol: Symbol, padding : Int = 2) : DateFormatHelper ={
    val parse_  : String=>(Symbol,String) = (string: String)=>(symbol,string)
    val format_ : Datum=>String           = (datum: Datum) =>
                                              datum.get(symbol).map(s"%0${padding}d".format(_)).getOrElse("#"*padding)

    DateFormatHelper(format=format_ , parse =parse_, placeholder = "(\d*)" )
  }

  def nam(symbol: Symbol, default : String = "UNKNOWN") : DateFormatHelper ={
    val parse_  : String=>(Symbol,String) = (string: String)=>(symbol,string)
    val format_ : Datum=>String           = (datum: Datum) =>
      datum.getName(symbol).getOrElse(default)

    DateFormatHelper(format=format_ , parse =parse_ , placeholder = "(.*)")
  }

  def y = num('year,4)
  def Y = nam('year,"#YEAR")
  def m = num('month)
  def M = nam('month,"#MONTH")
  def d = num('day)
  def D = nam('day,"#DAY")
  def h = num('hour)
  def H = nam('hour,"#HOUR")
  def min = num('minute)
  def Min = nam('minute,"#MINUTE")
  def s = num('second)
  def S = nam('second,"#SECOND")


}
