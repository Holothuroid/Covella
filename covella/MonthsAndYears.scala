package com.github.holothuroid.covella

trait MonthsAndYears{
  def days : TimeUnit

  def month(i: Int) =  'month of (days,i) withOffset 1

  def year(pairs: (Int,String) *) = {

    val months = pairs.map(x => month (x._1) )
    val names = pairs.map(_._2)

    'year isCycleOf  (months : _*) withNames (names : _*)
  }

}