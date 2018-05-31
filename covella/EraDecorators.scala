package com.github.holothuroid.covella

case class ExcludingShiftPrevious(decoratee: Era, excluded : Set[BigInt]) extends Decorator[Era] with Era{
  def jumps(i: BigInt) = excluded.count(_ >= i)

  override def decorate(datum: Datum) : Datum = datum.update(unitDesignation,_ match {
    case Ok(number,maybeName) => Ok(number-jumps(number),maybeName)
    case foo => foo
  })


  override def undecorate(datum: Datum) : Datum = {

    val newEntries = datum.entries.map {
      case (`unitDesignation`,Ok(number,maybeName)) => (unitDesignation,Ok(number+jumps(number),maybeName))
      case (`unitDesignation`,UncheckedIndex(number)) if excluded.contains(number) => (unitDesignation,Forbidden(number))
      case (`unitDesignation`,UncheckedPair(number,tentativeName)) if excluded.contains(number) => (unitDesignation,Forbidden(number))
      case (`unitDesignation`,UncheckedIndex(number)) => (unitDesignation,UncheckedIndex(number+jumps(number)))
      case (`unitDesignation`,UncheckedPair(number,tentativeName)) => (unitDesignation,UncheckedPair(number+jumps(number),tentativeName))
      case foo=>foo
    }

    datum.copy(entries = newEntries)
  }

  override def apply(i: BigInt) : Option[TimeUnit] = if(excluded.contains(i)) None else decoratee.apply(i+jumps(i))
  override def optimise: Era = ExcludingShiftPrevious(decoratee.optimise,excluded)
  override val unitDesignation: Symbol = decoratee.unitDesignation
  override def isDefinedAt(x: BigInt): Boolean = !excluded.contains(x) && decoratee.isDefinedAt(x+jumps(x))
  override def ticksOf(i: BigInt): BigInt = if(isDefinedAt(i)) decoratee.ticksOf(i+jumps(i)) else 0
  override def ticksUntil(i: BigInt): BigInt = decoratee.ticksUntil(i+jumps(i))
}


case class ExcludingShiftLater(decoratee: Era, excluded : Set[BigInt]) extends Decorator[Era] with Era{

  def jumps(i: BigInt) = excluded.count(_ <= i)

  override def decorate(datum: Datum) : Datum = datum.update(unitDesignation,_ match {
    case Ok(number,maybeName) => Ok(number+jumps(number),maybeName)
    case foo => foo
  })


  override def undecorate(datum: Datum) : Datum = {

    val newEntries = datum.entries.map {
      case (`unitDesignation`,Ok(number,maybeName)) => (unitDesignation,Ok(number-jumps(number),maybeName))
      case (`unitDesignation`,UncheckedIndex(number)) if excluded.contains(number) => (unitDesignation,Forbidden(number))
      case (`unitDesignation`,UncheckedPair(number,tentativeName)) if excluded.contains(number) => (unitDesignation,Forbidden(number))
      case (`unitDesignation`,UncheckedIndex(number)) => (unitDesignation,UncheckedIndex(number-jumps(number)))
      case (`unitDesignation`,UncheckedPair(number,tentativeName)) => (unitDesignation,UncheckedPair(number-jumps(number),tentativeName))
      case foo=>foo
    }

    datum.copy(entries = newEntries)
  }

  override def optimise: Era = ExcludingShiftLater(decoratee.optimise,excluded)
  override val unitDesignation: Symbol = decoratee.unitDesignation
  override def isDefinedAt(x: BigInt): Boolean = !excluded.contains(x) && decoratee.isDefinedAt(x-jumps(x))
  override def apply(i: BigInt) : Option[TimeUnit] =  if(excluded.contains(i)) None else decoratee.apply(i-jumps(i))
  override def ticksOf(i: BigInt): BigInt = if(isDefinedAt(i)) decoratee.ticksOf(i-jumps(i)) else 0
  override def ticksUntil(i: BigInt): BigInt = decoratee.ticksUntil(i-jumps(i))
}

