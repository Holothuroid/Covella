package com.github.holothuroid.covella


object Satellites extends Satellites(Vector(
  ("New","Crescent"),
  ("Half","Gibbous"),
  ("Full","Gibbous"),
  ("Half","Crescent")
)){

  def withPhases(phases : (String,String)*) = new Satellites(phases)

  def cycleDesignation(satelliteName: String) = Symbol(satelliteName + "Cycle")
  def phaseDesignation(satelliteName: String) = Symbol(satelliteName + "Phase")
  def visibilityDesignation(satelliteName: String) = Symbol(satelliteName + "Visibility")

  def vis(satelliteName: String) : DateFormat = nam(visibilityDesignation(satelliteName))
}


/**
  * This factory allows for creation of moons and similar satellites.
  * The satellites orbit will split into a number of phases, counting ticks.
  * This class' companion object is such a factory with the following four Phases
  *
  * ("New","Crescent"),("Half","Gibbous"),("Full","Gibbous"),("Half","Crescent")
  *
  * Each phase has a notable and a less notable visibility period,
  * which can be accessed from a resulting Datum via `.getName('xVisibility)`,
  * where x is the satellites name.
  *
  * To customize a Satellite factory, you can use `Satellites.withPhases((String,String)*)`.
  */

class Satellites (val phases: Seq[(String,String)]) {

  def phaseCount: Int = phases.size

  @inline
  def getVisibility(phase: Int, notable: Boolean) : String  =
     if(notable) phases(phase-1)._1 else phases(phase-1)._2

  /**
    * @param satelliteName The name is prefixed to all time units in the resulting Calendar.
    * @param cycleLength How many baseUnits does a single orbit take?
    * @param baseUnit In what unit does the cycle count? By default, CommonDays.days.
    * @param remainsNotable Allows determining, how long in `baseUnit`
    *                       the satellite will remain in a notable visibility state.
    * @return A SimpleCalendar with TimeUnits of the form
    *         `'moonCycle :: 'moonPhase :: 'moonPhaseSecond`
    *         assuming `satelliteName = "moon"` and a Tick with designation `'second`
    *         with an additional accesssor `'moonVisibility`.
    *         Phases start counting at 1.
    */

  def make( satelliteName: String,
            cycleLength: Double,
            baseUnit: TimeUnit = CommonDays.days,
            remainsNotable : Double = Double.PositiveInfinity
          ): SimpleCalendar = {

    val orbit : BigInt =  ( BigDecimal(baseUnit.ticks) * cycleLength ).toBigInt.abs
    val basePhaseLength = orbit/ phaseCount
    val remainder = orbit % phaseCount

    val cycleDesig = Satellites.cycleDesignation(satelliteName)
    val phaseDesig = Satellites.phaseDesignation(satelliteName)
    val visDesig = Satellites.visibilityDesignation(satelliteName)
    val localTickDesignation = Symbol(satelliteName + "Phase" +
      baseUnit.subunits.last.toString.tail.capitalize)

    val localTicks = Tick(localTickDesignation)

    val cycle =  if (remainder==0) {
      val phase = phaseDesig of(localTicks,basePhaseLength.toInt)
      cycleDesig of(phase, phaseCount) withOffset 1

    } else
    {
      val oneLonger = for (i <- 1 to remainder.toInt)
        yield phaseDesig of (localTicks,basePhaseLength.toInt+1)

      val rest = for (i <- remainder.toInt to phases.size)
        yield phaseDesig of(localTicks,basePhaseLength.toInt)

      Cycle(cycleDesig,oneLonger++rest) withOffset 1
    }

    def determineVisibility(datum: Datum) : String  = {
      val p : Int = datum.get(phaseDesig).get.toInt
      val notable : Boolean = datum.get(localTickDesignation).get <= baseUnit.ticks * remainsNotable.toLong
      getVisibility(p,notable)
    }

    ( Calendar(cycle) addName (visDesig -> determineVisibility) ) .asInstanceOf[SimpleCalendar]
  }

}
