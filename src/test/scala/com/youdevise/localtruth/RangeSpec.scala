package com.youdevise.localtruth

import org.specs2.mutable._

class RangeSpec extends Specification {

  "An open interval" should {

    val openInterval = Range(OpenBound(-10), OpenBound(10))

    "contain values between its endpoints" in {
      openInterval contains -9 should be equalTo(true)
      openInterval contains 0 should be equalTo(true)
      openInterval contains 9 should be equalTo(true)
    }

    "not contain its endpoints" in {
      openInterval contains -10 should be equalTo(false)
      openInterval contains 10 should be equalTo(false)
    }

    "not contain values beyond its endpoints" in {
      openInterval contains -11 should be equalTo(false)
      openInterval contains 11 should be equalTo(false)
    }
  }

  "A closed interval" should {

    val closedInterval = Range(ClosedBound(-10), ClosedBound(10))

    "contain values between its endpoints" in {
      closedInterval contains -9 should be equalTo true
      closedInterval contains 0 should be equalTo true
      closedInterval contains 9 should be equalTo true
    }

    "contain its endpoints" in {
      closedInterval contains -10 should be equalTo true
      closedInterval contains 10 should be equalTo true
    }

    "not contain values beyond its endpoints" in {
      closedInterval contains -11 should be equalTo false
      closedInterval contains 11 should be equalTo false
    }
  }

  "The intersection between two intervals" should {
    
    "be None if the intervals are not connected" in {
      val left = Range(OpenBound(0), OpenBound(1))
      val right = Range(OpenBound(1), OpenBound(2))
      
      left intersect right should be equalTo Set.empty
      right intersect left should be equalTo Set.empty
    }

    "be the overlap between both intervals" in {
      val left = Range(OpenBound(0), OpenBound(1))
      val right = Range(ClosedBound(1), OpenBound(2))

      left intersect right should be equalTo Set(Range(ClosedBound(1), OpenBound(1)))
      right intersect left should be equalTo Set(Range(ClosedBound(1), OpenBound(1)))
    }

    "be the inner interval if one contains the other" in {
      val innerInterval = Range(OpenBound(-10), ClosedBound(10))
      val outerInterval = Range(ClosedBound(-11), OpenBound(11))
      innerInterval intersect outerInterval should be equalTo Set(innerInterval)
      outerInterval intersect innerInterval should be equalTo Set(innerInterval)
    }
    
    "be an open interval when one is open and the other is closed, but both have the same endpoints" in {
      val openInterval = Range(OpenBound(-10), OpenBound(10))
      val closedInterval = Range(ClosedBound(-10), ClosedBound(10))
      openInterval intersect closedInterval should be equalTo Set(openInterval)
      closedInterval intersect openInterval should be equalTo Set(openInterval)
    }
  }

  "The union of two abutting intervals" should {
    val firstClosed = Range(ClosedBound(0), ClosedBound(1))
    val firstOpen = Range(OpenBound(0), OpenBound(1))
    val secondClosed = Range(ClosedBound(1), ClosedBound(2))
    val secondOpen = Range(OpenBound(1), OpenBound(2))

    "be a single interval when both are closed" in {
      firstClosed union secondClosed should be equalTo Set(Range(ClosedBound(0), ClosedBound(2)))
    }

    "be two separate intervals when both are open" in {
      firstOpen union secondOpen  should be equalTo Set(firstOpen, secondOpen)
    }

    "be a single interval when one is open but the other is closed at the abutting border" in {
      firstOpen union secondClosed should be equalTo Set(Range(OpenBound(0), ClosedBound(2)))
      firstClosed union secondOpen should be equalTo Set(Range(ClosedBound(0), OpenBound(2)))
    }
  }

  "The complement of one interval within another" should {
    "be the empty interval when the first contains the second" in {
      val outerInterval = Range(ClosedBound(-100), ClosedBound(100))
      val innerInterval = Range(OpenBound(-100), OpenBound(100))
      outerInterval complement innerInterval should be equalTo Set()
    }
    
    "be the inverse of the first when it is contained by the second" in {
      val innerInterval = Range(ClosedBound(-100), ClosedBound(100))
      val outerInterval = Range(OpenBound(-101), OpenBound(101))
      innerInterval complement outerInterval should be equalTo Set(Range(OpenBound(-101), OpenBound(-100)), Range(OpenBound(100), OpenBound(101)))
    }
    
    "be the inverse of the first when it overlaps the second" in {
      val left = Range(ClosedBound(0), ClosedBound(100))
      val right = Range(OpenBound(50), OpenBound(150))
      left complement right should be equalTo Set(Range(OpenBound(100), OpenBound(150)))
      right complement left should be equalTo Set(Range(ClosedBound(0), ClosedBound(50)))
    }
  }
}