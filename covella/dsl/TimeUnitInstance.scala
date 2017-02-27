/**
  * Created by 1of3 on 31.10.2016.
  */
package org.covella.dsl

import scala.collection.mutable
import scala.collection.mutable.HashMap


/**
  * This trait is for differing instances falling under the same TimeUnit.
  * If "month" is a TimeUnit, then this trait is for January, which is a month, but different from February or March.
  * It is not for that specific January, when it was really, really cold, which implements DateLike,
  * but for Januaries in general.
  */

trait TimeUnitInstance{
  val unit: TimeUnit
  val names: mutable.HashMap[String,String]= mutable.HashMap()
  val notes: collection.mutable.Set[String] = collection.mutable.Set.empty[String]

  def ticks: BigInt =1

  private var parent_ : Parenting = _
  def parent = parent_
  def setParent(par: Parenting) : this.type = {parent_ = par; this}

  private var index_ : BigInt = _
  def index = index_
  def setIndex(i: BigInt) : this.type ={index_ = i; this}

  def named(nam: String) : this.type = {names.update("default",nam); this}
  def aka(aliasKey: String, alias: String) : this.type = {names.update(aliasKey,alias); this}
  def note(not: String) : this.type = {notes.add(not); this}

  def + (that: TimeUnitInstance) = Seq(this,that)

  def name = names.applyOrElse("default", (string: String) => this.index)
  def alias(string: String) = names.applyOrElse(string, (string: String) => this.name)

  /**
    * @return The units of this instances direct children.
    */
  def childUnits: Set[TimeUnit] = Set()

  /**
    * @return A set of all this unit and all it's child and children's units.
    */
  def subunits: Set[TimeUnit] = Set(this.unit)

  /**
    * @return The subunits available everywhere, that is shared in all branches of the tree.
    */
  def counts: Set[TimeUnit] = Set(this.unit)


  override def toString: String = if( names.contains("default") ) "TUI(" + unit + " / " + names("default") + ")"
                                   else if(names.nonEmpty) "TUI(" + unit + " / " + names.values.head + ")"
                                   else "TUI(" + unit + ")"

}

/**
  * This trait is for TimeUnitInstances that contain other TimeUnitInstances, like months contain days.
 */

trait Parenting extends TimeUnitInstance {

  def getChild(index: BigInt): TimeUnitInstance

  var excluding: Set[BigInt] = Set()
  def exclude(values : BigInt*) : this.type = {excluding ++= values; this}
  def exclude(range : Range) : this.type = {for (i <- range) this exclude i; this}
  def isDefinedAt(index: BigInt)  : Boolean

  def byTicks(tocks: BigInt): (TimeUnitInstance, BigInt)
  def ticksUntil(index: BigInt): BigInt


}


/**
  * This trait specifies further settings for finite Parenting instances like months and years as opposed to potentially infinite Eras.
  */

trait ParentingFinite extends Parenting{

  /**
    * This method should return a TimeUnitInstance corresponding to this Instances n-th child for a non-negative index.
    * Whether the first child has index 0 or 1 depends on the TimeUnit (cf. TimeUnit#countFromZero).
    * If 1, the method should throw an exception for parameter 0.
    * Negative indices count from the end.
    * @param index Some Integer.
    * @return A TimeUnitInstance
    * @throws IllegalArgumentException, if the index is excluded (cf. TimeUnitInstance#excluding), exceeds maximum or is wrongly 0.
    */


  override def getChild(index: BigInt): TimeUnitInstance = if (excluding contains index)  throw new IllegalArgumentException(s"Index $index excluded from this $unit.")
  else if (index > maxIndex ) throw new IllegalArgumentException(s"Index $index is larger than this unit's maximum index ($maxIndex).")
  else if(index == 0 &&  (minIndex != 0 )) throw new IllegalArgumentException(s"Unit counts from 1. Called 0.")
  else if  (index < 0) {  getChildImpl(maxIndex + index + (if (minIndex != 0) 1 else 0))}
  else  getChildImpl(index)

  def getChildImpl(index:BigInt): TimeUnitInstance

  def firstChild = if (unit.countingFromZero) getChild(0) else getChild(1)
  val maxIndex: Int
  val minIndex: Int
  require (maxIndex > minIndex)
  lazy val domain = (minIndex to maxIndex).toSet diff excluding.map(_.toInt)
  def isDefinedAt(index: BigInt)  : Boolean = domain.contains(index.toInt)

  lazy val childLengths: Seq[BigInt] = for(i <- minIndex to maxIndex; if !excluding.contains(i)) yield getChild(i).ticks
  override def ticks = childLengths.sum
  lazy val partialLenghts: Seq[BigInt] = childLengths.scanLeft(BigInt(0))(_ + _)

  override def byTicks(tocks: BigInt): (TimeUnitInstance, BigInt) = {
    val cache = for (i <- partialLenghts.indices;
                     if partialLenghts(i) > tocks;
                     if partialLenghts(i+1) <= tocks)
                        yield (getChild(i + excluding.count(_<1) + (if(unit.countingFromZero) 0 else 1)), tocks - partialLenghts(i))
    cache.head }

  override def ticksUntil(index: BigInt) = partialLenghts(index.toInt) // Fehler abfangen!!!!

}



case class PrimitiveInstance(unit: TimeUnit) extends TimeUnitInstance


case class CompositeInstance(unit: TimeUnit,
                              children: Seq[TimeUnitInstance])
            extends TimeUnitInstance with ParentingFinite {

  require(children.nonEmpty)
  require(children.map(_.unit.countingFromZero).isEmpty || children.map(_.unit.countingFromZero).size == children.size)

  for(child <- children) child.setParent(this).setIndex(children.indexOf(child) +( if (children.head.unit.countingFromZero) 0 else 1))


  override def getChildImpl(index: BigInt) = children(index.toInt -( if (children.head.unit.countingFromZero) 2 else 1))
  override val minIndex = if(children.head.unit.countingFromZero) 0 else 1
  override val maxIndex = children.size - ( if(children.head.unit.countingFromZero) 1 else 0 )
  override def childUnits = children.map(_.unit).toSet
  override def subunits = children.map(_.subunits).reduce(_ union _) + this.unit
  override def counts = children.map(_.counts).reduce(_ intersect _) + this.unit
  override def toString: String = super.toString + " of {" + children.mkString(", ") + "}"
}

/**
  * A CycledInstance is an optimised TimeUnitInstance that does not actually store its children.
  * It can be used when all children are structurally the same, except for their index, names and notes.
  * Names and notes are stored in maps by index and are dynamically supplied, when a child is actually requested.
  *
  * @param unit       The unit of this CycledInstance
  * @param childUnit  The common unit of all children.
  * @param maxIndex   The length of this cycle.
  * @param childNames A map from indices to maps of names.
  * @param childNotes A map from indices to maps of notes.
  */

case class CycledInstance(unit: TimeUnit,
                          childUnit: Uniform,
                          maxIndex: Int,
                          childNames: mutable.Map[BigInt, mutable.HashMap[String, String]] = HashMap(),
                          childNotes: mutable.Map[BigInt, mutable.Set[String]] = HashMap()
                         ) extends ParentingFinite {

  /**
    * Alternative constructor via CycledInstanceBuilder
    *
    * @param unit          The unit of this CycledInstance
    * @param configuration A CycledInstanceBuilder
    * @return A CycledInstance (doh!)
    */

  def this(unit: TimeUnit, configuration: CycledInstanceBuilder) =
    this(unit,
      configuration.childUnit,
      configuration.size, configuration.childNames,
      configuration.childNotes)

  override def toString: String = super.toString + " of " + (maxIndex - excluding.size) + " " + childUnit + "s"


  override val minIndex = if (childUnit.countingFromZero) 0 else 1


  /* "Virtualising" certain methods from Parenting */

  override def getChildImpl(index: BigInt) // Regel für rückwärts zählen.
  = childUnit.newInstance(
    if (childNames.isDefinedAt(index)) childNames(index) else mutable.HashMap(),
    if (childNotes.isDefinedAt(index)) childNotes(index) else mutable.Set()
  ) setParent (this) setIndex (index)


  /* Optimisations for Parenting methods */

  override def childUnits: Set[TimeUnit] = Set(childUnit)
  override def subunits: Set[TimeUnit] = childUnit.subunits + this.unit
  override def counts: Set[TimeUnit] = childUnit.counts + this.unit

  override lazy val childLengths: Seq[BigInt] = for (i <-  minIndex to maxIndex - excluding.size)
    yield childLength

  lazy val childLength = childUnit.ticks
  override lazy val ticks = childLength * (maxIndex - excluding.size + (if (childUnit.countingFromZero) 1 else 0))

  override def byTicks(tocks: BigInt) = (getChild((tocks / childLength).toInt), tocks % childLength)
}

