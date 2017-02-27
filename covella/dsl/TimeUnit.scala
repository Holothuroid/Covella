/**
  * Created by 1of3 on 31.10.2016.
  */

package org.covella.dsl

import org.covella._
import org.covella.dates.Datum

import scala.collection.mutable
import scala.collection.mutable.HashMap

object TimeUnit{

  def named(nam: String) = new TimeUnit {override val designation = nam}

}

/**
  * Time units are a basic part of covella library. Think seconds, years.
  */

trait TimeUnit{
  /**
    * What is this unit called?
    */
  val designation: String

  /**
    * Boolean, true for units that start counting from 0, like hours, minutes or seconds.
    * As opposed to units that start with 1, like days, months.
    */
  private var countingFromZero_ : Boolean = false
  def countingFromZero = countingFromZero_
  def countFromZero: this.type  = {countingFromZero_ = true; this}
  override def toString: String = designation.toString

  /**
    * Creates a Datum, specifying this single TimeUnit.
    * You may want to use the Datum class's method & to add further units.
    * @param i This unit's index in its standard context
    * @return A Datum instance.
    */

  def >(i: BigInt) = Datum(Seq((this,i,InferedUnit)))
  def is(i: BigInt) = this> i

  /**
    * Allows specifying a context for constructing a Datum.
    * You can then use / on the resulting builder to add the index.
    * @param context A contest is something like "of week" in "day of week"
    * @return A static builder object.
    */

  def /(context: TimeUnit) = DatumEntryBuilder(this,context)
  def of(context: TimeUnit) = this/context

}


case class DatumEntryBuilder(first: TimeUnit, third: TimeUnit){
  def >(i: BigInt) : Datum = Datum(Seq((first,i,third)))
  def is(i: BigInt) = this > i
}


case class IrregularUnit(designation: String) extends TimeUnit




/**
  * This trait signifies TimeUnits that always have the same children. Like weeks containing always the same 7 days or
  * hours always containing 60 seconds. Also TimeUnits without children.
  * Known implementors: Tick, CompositeCycle, CCycle
  */

trait Uniform extends TimeUnit{

  type instanceType <: TimeUnitInstance

  def apply(i: Int) = { require(i>0); CycledInstanceBuilder(this,i) }
  def apply(nams: String*) = {  val values = nams map ( (x: String) => HashMap[String,String]("default"-> x) )
                                var names = new mutable.HashMap[BigInt,HashMap[String,String]]()
                                for (key <- values.indices; value <- values ) names.update(key, value)
                                for (key <- values.indices; value <- values ) names.update(key, value)

                                CycledInstanceBuilder(this,nams.size,names) }


  def newInstance(names: HashMap[String,String] = HashMap(),
                  notes: collection.mutable.Set[String] = mutable.Set()): instanceType

  def counts : Set[TimeUnit]
  def subunits : Set[TimeUnit]
  def ticks: BigInt

}


/**
  * This object is an abstract factory for cycles. Cycles are Uniform time units that do have children.
  */

object Cycle{

  implicit def cycle2instance(cycle: Cycle) : Parenting = cycle.newInstance()

  def from(children: Seq[TimeUnitInstance]) = CompositeCycleBuilder(children)

  def from(configuration: CycledInstanceBuilder) = CCycleBuilder(configuration)

  case class CompositeCycleBuilder(children: Seq[TimeUnitInstance]){
    def named(nam: String) = CompositeCycle(nam,children)
  }

  case class CCycleBuilder(configuration: CycledInstanceBuilder){
    def named(nam: String) = CCycle(nam,configuration)
  }


}

trait Cycle extends Uniform {

  override type instanceType = ParentingFinite

}


/**
  * This cycle's children form a Seq[TimeUnitInstance].
  *
  */

case class CompositeCycle(override val designation: String, children: Seq[TimeUnitInstance])
  extends  Cycle {
  require(children.nonEmpty)


  override def newInstance(nams: HashMap[String,String] = HashMap(),
                           nots: collection.mutable.Set[String] = mutable.Set())
          = new CompositeInstance(this,children)
            {override val names = nams; override val notes = nots }

  override lazy val counts: Set[TimeUnit] = children.map(_.counts).reduce(_ intersect _) + this
  override lazy val subunits: Set[TimeUnit] = children.map(_.counts).reduce(_ union _) + this
  override lazy val ticks = children.map(_.ticks).sum
}


/**
  * This cycle's children form another cycle.
 */

case class CCycle(override val designation: String, configuration: CycledInstanceBuilder)
  extends Cycle {

  override def newInstance(nams: mutable.HashMap[String,String] = mutable.HashMap(),
                           nots: collection.mutable.Set[String] = mutable.Set())
          = new CycledInstance(this,configuration)
              {override val names = nams; override val notes = nots }

  override lazy val counts: Set[TimeUnit] = configuration.childUnit.counts + configuration.childUnit + this
  override lazy val subunits: Set[TimeUnit] = counts
  override lazy val ticks = configuration.childUnit.ticks * configuration.size
}




case class Tick(designation: String, override val countingFromZero: Boolean = false) extends TimeUnit with Uniform {

  override type instanceType = PrimitiveInstance

  override def newInstance(nams: mutable.HashMap[String, String] = mutable.HashMap(),
                           nots: mutable.Set[String] = mutable.Set())
  = new PrimitiveInstance(this) {override val names = nams; override val notes = nots}

  override def subunits: Set[TimeUnit] = Set(this)
  override def counts: Set[TimeUnit] = Set(this)
  override def ticks = 1
}



case object InferedUnit extends TimeUnit{
  override val designation = "InferedUnit"
}


/**
  * This builder class creates a CycledInstance, when CycledInstancBuilder::as is called.
  * Get a builder by calling Uniform::apply.
  * @param childUnit  The Uniform unit the CycledInstance should contain.
  * @param size The length of the unit to be built.
  * @param childNames The names for the virtual children.
  * @param childNotes The notes for the virtual children.
  */



case class CycledInstanceBuilder(childUnit: TimeUnit with Uniform,
                                 var size: Int,
                                 childNames: mutable.Map[BigInt,HashMap[String,String]] = HashMap(),
                                 childNotes: mutable.Map[BigInt,mutable.Set[String]] = HashMap() ){

  def +(tui: TimeUnitInstance) : Seq[TimeUnitInstance] = this.unpacked :+ tui
  def +(that: CycledInstanceBuilder) : Seq[TimeUnitInstance] = this.unpacked ++ that.unpacked

    /*if (this.childUnit != that.childUnit) this.unpacked ++ that.unpacked
                else {  val newNames : mutable.Map[BigInt,HashMap[String,String]]
                                          = that.childNames map { case (x,y) => (x + this.size,y) }
                        val newNotes : mutable.Map[BigInt,collection.mutable.Set[String]]
                                                = that.childNotes map {case (x,y) => (x + this.size,y)}

                        CycledInstanceBuilder(childUnit, this.size + that.size,
                              this.childNames++newNames, this.childNotes ++ newNotes) } */


  def +(seq: Seq[TimeUnitInstance]) = this.unpacked() ++ seq

  def unpacked() : Seq[TimeUnitInstance] = {
     val start = if (childUnit.countingFromZero) 0 else 1
     for (i <- start to size) yield childUnit.newInstance(
                                         if (childNames isDefinedAt i) childNames(i) else HashMap(),
                                        if (childNotes isDefinedAt i ) childNotes(i) else mutable.Set())
                                         }

  /**
    * Use this method to finish the builder and create a CycledInstance.
    * @param iu An IrregularUnit.
    */

  def as(iu: IrregularUnit) : CycledInstance = CycledInstance( unit = iu, childUnit = this.childUnit,
                                  maxIndex = this.size,
                                  childNames = this.childNames,
                                  childNotes = this.childNotes )

}

