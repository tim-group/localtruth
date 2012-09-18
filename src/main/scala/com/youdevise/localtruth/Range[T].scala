package com.youdevise.localtruth

abstract case class Bound[T]()(implicit ord:Ordering[T]) {
  def boundsUpper(point:T):Boolean
  def boundsLower(point:T):Boolean
  def inverse:Bound[T]
}
case class OpenBound[T](endpoint:T)(implicit ord:Ordering[T]) extends Bound[T] {
  override def boundsUpper(point:T):Boolean = ord.compare(point, endpoint) < 0
  override def boundsLower(point:T):Boolean = ord.compare(point, endpoint) > 0
  override def inverse = ClosedBound(endpoint)
}

case class ClosedBound[T](endpoint:T)(implicit ord:Ordering[T]) extends Bound[T] {
  override def boundsUpper(point:T):Boolean = ord.compare(point, endpoint) <= 0
  override def boundsLower(point:T):Boolean = ord.compare(point, endpoint) >= 0
  override def inverse = OpenBound(endpoint)
}

case class Unbounded[T]()(implicit ord:Ordering[T]) extends Bound[T] {
  override def boundsUpper(point:T):Boolean = true
  override def boundsLower(point:T):Boolean = true
  override def inverse = Unbounded()
}

case class Range[T](val lower: Bound[T], val upper: Bound[T]) {

  def contains(point:T):Boolean = (lower boundsLower point) && (upper boundsUpper point)
  def contains(other:Range[T]):Boolean = intersect(other).headOption.map(_ == other).getOrElse(false)
  
  def contains(bound:Bound[T]):Boolean = bound match {
    case Unbounded() => false
    case OpenBound(endpoint) => this contains endpoint
    case ClosedBound(endpoint) => this contains endpoint
  }
  
  def connectedTo(other:Range[T]):Boolean = ((this contains other.lower) || (this contains other.upper)
                                            || (other contains lower) || (other contains upper))

  def leastLowerBound(other:Range[T]):Bound[T] = other.lower match {
    case Unbounded() => other.lower
    case OpenBound(endpoint) => if (this.lower boundsLower endpoint) this.lower else other.lower
    case ClosedBound(endpoint) => if (this.lower boundsLower endpoint) this.lower else other.lower
  }

  def greatestUpperBound(other:Range[T]):Bound[T] = other.upper match {
    case Unbounded() => other.upper
    case OpenBound(endpoint) => if (this.upper boundsUpper endpoint) this.upper else other.upper
    case ClosedBound(endpoint) => if (this.upper boundsUpper endpoint) this.upper else other.upper
  }

  def greatestLowerBound(other:Range[T]):Bound[T] = this.lower match {
    case Unbounded() => other.lower
    case OpenBound(endpoint) => if (other.lower boundsLower endpoint) this.lower else other.lower
    case ClosedBound(endpoint) => if (other.lower boundsLower endpoint) this.lower else other.lower
  }

  def leastUpperBound(other:Range[T]):Bound[T] = this.upper match {
    case Unbounded() => other.upper
    case OpenBound(endpoint) => if (other.upper boundsUpper endpoint) this.upper else other.upper
    case ClosedBound(endpoint) => if (other.upper boundsUpper endpoint) this.upper else other.upper
  }
  
  def intersect(other:Range[T]):Set[Range[T]] = if (this connectedTo other)
      Set(Range(greatestLowerBound(other), leastUpperBound(other)))
      else Set.empty
  
  def union(other:Range[T]):Set[Range[T]] = if (this connectedTo other)
      Set(Range(leastLowerBound(other), greatestUpperBound(other)))
      else Set(this, other)
  
  def complement(other:Range[T]):Set[Range[T]] = {
    if (this contains other) return Set.empty
    if (!(this connectedTo other)) return Set(other)
    if (other contains this) return Set(Range(other.lower, this.lower.inverse), Range(this.upper.inverse, other.upper))
    if (other contains this.lower) return Set(Range(other.lower, this.lower.inverse))
    return Set(Range(this.upper.inverse, other.upper))
  }
}

object Ranges {
  implicit def iterableOfRanges2RangeSet[T](ranges:Iterable[Range[T]])(implicit ord:Ordering[T]):RangeSet[T] = new RangeSet[T](ranges)
  def closed[T](lower:T, higher:T)(implicit ord:Ordering[T]) = Range(ClosedBound(lower), ClosedBound(higher))
  def open[T](lower:T, higher:T)(implicit ord:Ordering[T]) = Range(OpenBound(lower), OpenBound(higher))
  def unbounded[T](implicit ord:Ordering[T]) = Range(Unbounded[T](), Unbounded[T]())
}

class RangeSet[T](val uncoalesced:Iterable[Range[T]])(implicit ord:Ordering[T]) {
  lazy val coalesced:Set[Range[T]] = {
    if (uncoalesced.size < 2) {
      uncoalesced.toSet
    } else {
      val ordered = uncoalesced.toList.sortWith(rangeOrder)
      var left = ordered.head
      val iter = ordered.iterator.drop(1)
      val result = scala.collection.mutable.Set[Range[T]]()
      while (iter.hasNext) {
        val right = iter.next()
        if (left connectedTo right) {
          left = (left union right).head
        } else {
          result add left
          left = right
        }
      }
      result add left
      Set(result.toSeq:_*)
    }
  }
  
  val rangeOrder = (x:Range[T], y:Range[T]) =>
      if (x.lower == y.lower) (x.upper != y.upper) && (x.leastUpperBound(y) == x.upper) else (x.leastLowerBound(y) == x.lower)

  def containsPoint(point:T):Boolean = coalesced.exists(_.contains(point))

  def intersect(other:RangeSet[T]):RangeSet[T] = {
    val intersections = for {
      x <- coalesced;
      y <- other.coalesced;
      z <- (x intersect y)
    } yield z
    return new RangeSet(intersections)
  }

  override def hashCode:Int = coalesced.hashCode()
  override def equals(other:Any):Boolean = other match {
    case otherRangeSet:RangeSet[T] => otherRangeSet.coalesced == coalesced
    case _ => false
  }
  override def toString:String = coalesced.toString()
}