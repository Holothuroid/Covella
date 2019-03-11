package com.github.holothuroid.covella

/**
  * This class describes an interval of time.
  *
  * @param starts Timelike when it starts.
  * @param ends Timelike when it ends.
  */

case class Interval(starts: Timelike, ends: Timelike)  {
  require (starts <= ends)

  def contains(t: Timestamp) = starts <= t && t <= ends
  def contains(i: Interval) = starts <= i.starts && i.ends <= ends
  def overlaps(that: Interval) = (this contains that.starts) || (this contains that.ends)
  def < (that: Interval) = this.ends < that.starts

  def intersect(that: Interval) : Option[Interval] =
    if (!(this overlaps that)) None
    else if (that.starts<=this.ends) Some(Interval(that.starts,this.ends))
    else   Some(Interval(this.starts,that.ends))

  def before(t: Timelike) : Option[Interval] = if (this < t) Some(this)
    else if (t < this) None
      else Some(Interval(this.starts,t))

  def after(t: Timelike) : Option[Interval] = if (t < this) Some(this)
    else if (this < t) None
      else Some(Interval(t,this.ends))
}

/**
  * DateQuery is a small DSL by algebraic data types, to describe intervals.
  */

//
//trait DateQuery {
//  def execute(implicit cal: Calendar) : ISet[Interval]
//
//  def when(that: DateQuery) = When(this,that)
//  def before(that: DateQuery) = Before(this,that)
//  def after(that: DateQuery) = After(this,that)
//  def afterBegins(that: DateQuery) = AfterBegins(this,that)
//  def beforeEnds(that: DateQuery) = BeforeEnds(this,that)
//}
//
//object DateQuery {
//  implicit def datum2Lifted(d: Datum) : Lifted = Lifted(d)
//  implicit def timestamp2Fixpoint(t: Timestamp) : Fixpoint = Fixpoint(t)
//}
//
//
//case class Lifted(datum: Datum) extends DateQuery { // todo: Dangling dates should become real sets.
//  def execute(implicit cal: Calendar) : ISet[Interval] = ISet[Interval]{ case Interval(a: Timestamp,b : Timestamp) => datum.matches(a) && datum.matches(b) }
////    (for (starts <- datum.begins; ends <- datum.ends)
////      yield Interval(starts,ends)).toSet
//
//}
////
////case class Fixpoint(value: Timestamp) extends DateQuery {
////  def execute(implicit cal: Calendar) : ISet[Interval] = Set(Interval(value,value))
////
////}
////
////
////trait CombinedQuery extends DateQuery {
////
////  val a: DateQuery
////  val b: DateQuery
////
////
//}
//
//case class When(a : DateQuery, b: DateQuery) extends CombinedQuery {
//  def execute(implicit cal: Calendar): ISet[Interval] =
//     for ( intervalA <- a.execute(cal);
//           intervalB <- b.execute(cal);
//           maybeInterval <- intervalA.intersect(intervalB))
//      yield maybeInterval
//
//
//  }
//
//
//case class Before(a : DateQuery, b: DateQuery)extends CombinedQuery  {
//  def execute(implicit cal: Calendar): Set[Interval] = {
//    val limit = b.execute(cal).map(_.starts).min
//    a.execute(cal).flatMap(_.before(limit))
//  }
//
//}
//
//case class After(a : DateQuery, b: DateQuery)extends CombinedQuery  {
//  def execute(implicit cal: Calendar): Set[Interval] = {
//    val limit = b.execute(cal).map(_.ends).max
//    a.execute(cal).flatMap(_.after(limit))
//  }
//
//}
//
//case class AfterBegins(a : DateQuery, b: DateQuery)extends CombinedQuery  {
//  def execute(implicit cal: Calendar): Set[Interval] = {
//    val limit = b.execute(cal).map(_.starts).min
//    a.execute(cal).flatMap(_.after(limit))
//  }
//
//}
//
//case class BeforeEnds(a : DateQuery, b: DateQuery)extends CombinedQuery  {
//  def execute(implicit cal: Calendar): Set[Interval] = {
//    val limit = b.execute(cal).map(_.ends).max
//    a.execute(cal).flatMap(_.before(limit))
//  }
//
//}
//
