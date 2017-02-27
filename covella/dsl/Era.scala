package org.covella.dsl

import org.covella.dsl


/**
  * An Era is a special type of TimeUnitInstance and commonly used as a top-level element in a Calendar.
  * It does not have a fixed set of children, but will return the corresponding TimeUnitInstance,
  * if the given index passes a test.
  *
  * It doesn't have finite length.
  */




object Era {
  val eraUnit = TimeUnit named "eraUnit"



  type Check = BigInt => Boolean
  def given(test: Check) = EraArgBuilder(test)
  def given(i0: Int*) = EraArgBuilder(i0 contains _)
  def rest (tui: TimeUnitInstance) : Era = Era(Seq(((_ : BigInt) => true,tui)))

  case class EraArgBuilder(predicate: Check,
                           args: Seq[(Check, TimeUnitInstance)] = Seq(),
                           var unit: TimeUnit = eraUnit,
                           var excluding: Set[BigInt] = Set()
                          ) {

    def have(tui: TimeUnitInstance) = Era(args :+ ((predicate, tui)), unit, excluding)


  }

}



case class Era (args: Seq[(Era.Check , TimeUnitInstance)],
                unit : TimeUnit = dsl.Era.eraUnit,
                alreadyExcluding : Set[BigInt]  =Set()

               )
  extends TimeUnitInstance with Parenting
{

  import Era._

  this.excluding = alreadyExcluding
  def checks = args.map(_._1)
  def values = args.map(_._2)
  def given(test: Check) = EraArgBuilder(test,args,unit,excluding)
  def given(i0: Int*) = EraArgBuilder(i0 contains _,args,unit,excluding)
  def rest(tui: TimeUnitInstance): Era = Era (args :+ ((x: BigInt) => true,tui),
                                              unit,
                                              alreadyExcluding)




  override def getChild(index: BigInt): TimeUnitInstance = {
    val child = checks.map(_.apply(index)).indexOf(true) // hier muss eine Catch rein!!!
    val output = values(child) setIndex(index) setParent(this)
    for(key <- eraNames(index).keySet) output.names.update(key,eraNames(index)(key))
    for(note <- eraNotes(index)) output.notes.add(note)

    output
    }

  override def childUnits: Set[TimeUnit] = values.map(_.unit).toSet
  override def counts: Set[TimeUnit] = values.map(_.counts).reduce(_ intersect _ )
  override def subunits: Set[TimeUnit] = values.map(_.subunits).reduce(_ union _ )


  private var eraNames : BigInt => Map[String,String] = ((i: BigInt) => Map("default" -> i.toString) )
  def setEraNames(function : BigInt=> Map[String,String]) : this.type = {eraNames = function; this}
  def defaultEpochNames(ante: String, post: String) = setEraNames ( x =>  Map( "default" ->  (if (x>=0) x + " " + post.trim else x + " " + ante.trim) ) )

  private var eraNotes : BigInt => Set[String] = ((i: BigInt) => Set() )
  def setEraNotes(function : BigInt=> Set[String]) : this.type = {eraNotes = function; this}

  override def isDefinedAt(index: BigInt) = !excluding.contains(index)

  override def byTicks(tocks: BigInt) = {
    def forwards(tocks: BigInt, current: BigInt) : (TimeUnitInstance,BigInt) =
          if (tocks > getChild(current).ticks && tocks <= getChild(current+1).ticks)
              (getChild(current +  excluding.filter(i => i>=0 && i<current).size),ticksUntil(current))
          else forwards(tocks,current+1)

    def backwards(tocks: BigInt, current: BigInt) : (TimeUnitInstance,BigInt) =
      if (tocks < getChild(current).ticks && tocks >= getChild(current-1).ticks)
            (getChild(current +  excluding.filter(i => i<0 && i>=current).size ) ,ticksUntil(current))
      else forwards(tocks,current-1)

      if (tocks >= 0) forwards(tocks,0)
    else backwards(tocks,-1)
  }



  override def ticksUntil(index: BigInt) : BigInt = if (index>=0){
        def streamy(x: BigInt,y: BigInt) : BigInt = if (x == index) y else streamy(x+1,y+getChild(x+1).ticks)
         streamy(BigInt(0),getChild(0).ticks)
     }
      else { def streamy(x: BigInt,y: BigInt) : BigInt = if (x == index) y else streamy(x-1,y-getChild(x-1).ticks)
               streamy(BigInt(-1),getChild(-1).ticks)}


  }


