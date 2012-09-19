package com.youdevise.localtruth

import org.specs2.mutable._
import com.youdevise.localtruth.Ranges._

class RangeSetSpec extends Specification {

  "A range set" should {
    "coalesce an empty List of ranges into an empty List" in {
      List[Range[Int]]().coalesced should be equalTo List[Range[Int]]()
    }

    "coalesce adjacent ranges" in {
      List(closed(0, 3), closed(3, 5)).coalesced should be equalTo List(closed(0, 5))
    }

    "coalesce overlapping ranges" in {
      List(closed(0, 2), closed(1, 3)).coalesced should be equalTo List(closed(0, 3))
    }

    "coalesce contained ranges" in {
      List(closed(0, 5), closed(1, 4)).coalesced should be equalTo List(closed(0, 5))
    }

    "coalesce multiple connected ranges" in {
      List(closed(0, 1), closed(3, 4), closed(1, 2), closed(4, 5), closed(2, 3)).coalesced should be equalTo List(closed(0, 5))
    }

    "not coalesce non-connected ranges" in {
      List(open(0, 1), open(1, 2)).coalesced should be equalTo List(open(0, 1), open(1, 2))
    }

    "coalesce discrete groups of connected ranges separately" in {
      List(closed(0, 1), closed(9, 10), closed(8, 9), closed(1, 2)).coalesced should be equalTo List(closed(0, 2), closed(8, 10))
    }

    "coalesce ranges with the same lower bound" in {
      List(closed(0, 7), closed(10, 12), closed(0, 4), closed(10, 13)).coalesced should be equalTo List(closed(0, 7), closed(10, 13))
    }
    
    "coalesce all ranges with an unbounded range" in {
      List(unbounded[Int], closed(0, 3), closed(-10000, 10000)).coalesced should be equalTo List(unbounded[Int])
    }

    "contain any value that is contained in any of its component ranges" in {
      List(closed(0, 2), closed(5, 7)) containsPoint 1 should be equalTo true
      List(closed(0, 2), closed(5, 7)) containsPoint 6 should be equalTo true
      List(closed(0, 2), closed(5, 7)) containsPoint 4 should be equalTo false
      List(closed(0, 2), closed(5, 7)) containsPoint 8 should be equalTo false
    }
  }

  "The intersection of two range sets" should {
    "contain the intersections of every range in List A with every range in List B" in {
      val setA:RangeSet[Int] = List(closed(0, 2), closed(5, 10), closed(15, 20))
      val setB:RangeSet[Int] = List(closed(1, 3), closed(4, 8), closed(8, 9), closed(9, 13), closed(14, 16))
      val intersection = setA intersect setB
      intersection should be equalTo List(closed(1,2), closed(5,8), closed(8,9), closed(9, 10), closed(15, 16))
    }
  }

}