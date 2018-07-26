package com.github.holothuroid.covella


import scala.language.implicitConversions



/**
  * A human-ish representation of dates, consisting of qualified key-value pairs.
  * @param entries Maps Symbols to DatumEntries.
  * @param cal Optionally, a calendar according to which the DatumEntries are chosen.
  */

case class Datum(entries: Map[Symbol,DatumEntry] = Map(),cal: Option[Calendar] = None){
  import entries.{isDefinedAt, values}


  def this(tuples : (Symbol,DatumEntry)*)  = this(tuples.toMap)

  /**
    * The method combines two Datums. It takes all the units from this object, as well as its calendar, if present.
    * It adds those units from the second datum, that are not found in the first.
    *
    * @param that The Datum to be added
    * @return A new Datum.
    */
  def & (that: Datum) : Datum =  Datum(that.entries ++ this.entries,this.cal)

  // Getters
  def apply(symbol: Symbol) : Option[DatumEntry] = entries.get(symbol)
  def get(symbol: Symbol) : Option[BigInt] = entries.get(symbol).flatMap(_.value)
  def getName(symbol: Symbol) : Option[String] = entries.get(symbol).flatMap(_.name)
  def getOrElse(symbol: Symbol, default: BigInt) : BigInt = get(symbol).getOrElse(default)
  def getNameOrElse(symbol: Symbol, default: String) : String = getName(symbol).getOrElse(default)


  // Methods creating new Datums.
  def remove(tus: Iterable[Symbol] ) : Datum = this.copy(entries = this.entries -- tus)
  def update(tu: Symbol, entry: DatumEntry) : Datum = this.copy(entries = this.entries.updated(tu,entry))

  def update(tu: Symbol, modification: DatumEntry=>DatumEntry) : Datum =
    apply(tu).map(modification) match {
       case Some(newEntry) => this.copy(entries = this.entries.updated(tu,newEntry))
       case None => this
     }

  /**
    * Sets this Datum to the target calendar, then checks the Datum.
    * If Datum is without error, it should be completed by all information provided by the given calendar.
    * @param cal The target calendar.
    * @return The so constructed Datum.
    */

  def completeWithCalendar(implicit cal: Calendar): Datum = {
    val uncheckedEntries = entries.map{ case (tu,entry) => (tu,entry.reset) }

    val checked = cal.check(Datum(uncheckedEntries))

    if (checked.hasError) checked
    else checked.begins.map(_.inCalendar(cal)).getOrElse(Datum(Map(),Some(cal)))
  }


  // Predicates
  def hasError : Boolean = values.exists(_.isInstanceOf[DatumError])
  def isOk : Boolean = values.forall(_.isInstanceOf[Ok])
  def isOkAt(unit: Symbol) = isDefinedAt(unit) && entries(unit).isInstanceOf[Ok]
  def isOkUntil : Option[Symbol] = cal.flatMap(_.primaryUnits.takeWhile(this.isOkAt(_)).lastOption)
  def isComplete = isOkUntil.nonEmpty && isOkUntil == cal.map(_.units.last)


  /**
    * Checks wheter two Datums match, by the following critera:
    * - A Datum containing one or more DatumErrors matches nothing.
    * - Otherwise all useful information contained in both Datums must be equal.
    * - Information found in only one of the Datums is ignored.
    * - Whether or not the Datums point to a Calendar, doesn't matter for the result.
    * - The method is symmetric.
    * - Hence, the empty Datum matches all Datums without errors.
    * @param that Another Datum.
    * @return Boolean.
    */

  def matches(that: Datum) : Boolean ={
    if (this.hasError || that.hasError) return false

    (this.entries.keySet intersect that.entries.keySet).forall { key =>
      val thisValue = this.entries(key).value
      val thatValue = this.entries(key).value
      val thisName = this.entries(key).name
      val thatName = this.entries(key).name

      (thisValue.isEmpty || thatValue.isEmpty || thisValue==thatValue) &&
        (thisName.isEmpty || thatName.isEmpty || thisName==thatName)
    }
  }


  // Methods creating other kinds of objects.
  def format(implicit df: DateFormat) : String = df.format(this)

  lazy val begins : Option[Timestamp] = cal.flatMap(_.timestampOrZero(this))

  def ends : Option[Timestamp] = {
    if (cal.isEmpty || begins.isEmpty) return None
    if (timestamp.nonEmpty) return timestamp



  }

  lazy val timestamp : Option[Timestamp] = cal.flatMap(_.timestamp(this))
  def interval : Option[Interval] = for(start <- begins; end <-ends if start<=end) yield Interval(start,end)


  override def toString: String = {
    val start = "Datum(" + entries.mkString(", ")
    cal match {
      case Some(calendar) => start + s"; $calendar)"
      case None => start + ")"
    }
  }

}


object Datum{
  def of(pairs: (Symbol,BigInt)*) = {
    val entries = pairs.map { case (symbol,i) => (symbol,UncheckedIndex(i)) }
    Datum(entries.toMap)
  }

  def ofNames(pairs: (Symbol,String)*) = {
    val entries = pairs.map { case (symbol,s) => (symbol,UncheckedName(s)) }
    Datum(entries.toMap)
  }

  def ofPairs(pairs: (Symbol,(BigInt,String))*) = {
    val entries = pairs.map { case (symbol,(i,s)) => (symbol,UncheckedPair(i,s)) }
    Datum(entries.toMap)
  }

}







sealed trait DatumEntry{
  def value: Option[BigInt] = None
  def name: Option[String] = None
  def reset: Unchecked

  private def inBounds(tentativeIndex: BigInt,lower: BigInt,upper: BigInt, forbidden: BigInt=>Boolean) : DatumEntry = {
    require(lower < upper)
    if (tentativeIndex < lower) TooLow(tentativeIndex,lower)
    else if (tentativeIndex > upper) TooHigh(tentativeIndex,upper)
    else if (forbidden(tentativeIndex)) Forbidden(tentativeIndex)
    else Ok(tentativeIndex)
  }

  def requireValue(lower: BigInt, upper: BigInt, forbidden: BigInt=>Boolean = _=>false): DatumEntry = {
    this match {
      case e: DatumError => e
      case p: PotentiallyCorrect =>  (p.value,p.name) match{
        case (Some(i),_) => inBounds(i,lower,upper,forbidden)
        case (None,Some(s)) => Unknown(s)
        case (None,None) => throw new IllegalStateException("DatumEntry has neither value nor name; and is not an error.")
      }
    }
  }

  def requireAllowed(forbidden: BigInt=>Boolean): DatumEntry =
    this match {
      case e: DatumError => e
      case p: PotentiallyCorrect =>  (p.value,p.name) match{
        case (Some(i),_) => if (forbidden(i)) Forbidden(i) else Ok(i)
        case (None,Some(s)) => Unknown(s)
        case (None,None) => throw new IllegalStateException("DatumEntry has neither value nor name; and is not an error.")
      }
    }


}

sealed trait PotentiallyCorrect extends DatumEntry


case class Ok(index: BigInt,override val name: Option[String] = None) extends PotentiallyCorrect{
  override def value  = Some(index)

  override def reset: Unchecked = name match {
    case Some(s) => UncheckedPair(index,s)
    case None => UncheckedIndex(index)
  }
}

trait Unchecked extends PotentiallyCorrect{
  override def reset = this
}

case class UncheckedIndex(tentativeIndex: BigInt) extends Unchecked{
  override def value = Some(tentativeIndex)
}

case class UncheckedName(tentativeName: String) extends Unchecked{
  override def name = Some(tentativeName) }

case class UncheckedPair(tentativeIndex: BigInt,tentativeName: String) extends Unchecked{
  override def value = Some(tentativeIndex)
  override def name = Some(tentativeName) }


trait DatumError extends DatumEntry

trait NumericError extends DatumError{
  def tentativeIndex : BigInt
  override def reset = UncheckedIndex(tentativeIndex)
}

case class TooHigh(tentativeIndex: BigInt, upperBound: BigInt) extends NumericError
case class TooLow(tentativeIndex: BigInt, upperBound: BigInt) extends NumericError
case class Forbidden(tentativeIndex: BigInt) extends NumericError
case class NameNotFound(tentativeName: String) extends DatumError{
  override def reset = UncheckedName(tentativeName)
}
case class Incongruent(tentativeIndex: BigInt,tentativeName: String) extends DatumError{
  override def reset: Unchecked = UncheckedPair(tentativeIndex,tentativeName)
}

case class Unknown(stuff: String) extends DatumError {
  override def reset = UncheckedName(stuff)
}

case class Extra( override val value: Option[BigInt],
                override val name: Option[String] ) extends Unchecked
