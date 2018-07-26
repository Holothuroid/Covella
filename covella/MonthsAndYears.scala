package com.github.holothuroid.covella

trait MonthsAndYears{
  def days : TimeUnit
  def prefix : String = ""


  def prefixSymbol(prefix: String, symbol: Symbol) : Symbol = Symbol(prefix + symbol.toString.tail.capitalize)
  def localDays : TimeUnit = if (prefix.isEmpty) days else prefixSymbol(prefix,days.designation) isAliasFor days
  def localMonthSymbol : Symbol = if (prefix.isEmpty) 'month else prefixSymbol(prefix,'month)
  def localYearSymbol : Symbol = if (prefix.isEmpty) 'year else prefixSymbol(prefix,'year)

  def month(i: Int) =   localMonthSymbol of (localDays,i) withOffset 1

  def year(pairs: (Int,String) *) = {

    val months = pairs.map(x => month (x._1) )
    val names = pairs.map(_._2)

    localYearSymbol isCycleOf  (months : _*) withNames (names : _*) withOffset 1
  }

  def year(months: Seq[TimeUnit], names: Seq[String] = Vector()) = {

    if (names.isEmpty) localYearSymbol isCycleOf  (months : _*)  withOffset 1
    else localYearSymbol isCycleOf  (months : _*) withNames (names : _*) withOffset 1
  }

}