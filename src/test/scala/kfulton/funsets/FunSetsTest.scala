package kfulton.funsets

import org.scalatest.{FlatSpec, Matchers}

class FunSetsTest extends FlatSpec with Matchers {
  import FunSets._
  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)

  "contains" should "return true if a set contains given element" in {
    contains(x => true, 100) shouldBe true
  }

  it should "return false if it does not contain the element" in {
    contains(x => false, 100) shouldBe false
  }

  "singletonSet1" should "contain 1" in {
    contains(s1, 1) shouldBe true
    contains(s1, 2) shouldBe false
  }

  "union" should "contain all the elements of each set" in {
    val uSet = union(s1, s2)
    contains(uSet, 1) shouldBe true
    contains(uSet, 2) shouldBe true
    contains(uSet, 3) shouldBe false
  }

  "intersect" should "contain elements that appear in both sets" in {
    val iSet = intersect(s1, s1)
    contains(iSet, 1) shouldBe true
    val iSet2 = intersect(s1,s2)
    contains(iSet2, 1) shouldBe false
    contains(iSet2, 2) shouldBe false
  }

  "diff" should "contain elements that do not appear in both sets" in {
    val dSet = diff(s1, s2)
    contains(dSet, 1) shouldBe true
    contains(dSet, 2) shouldBe true
    contains(dSet, 3) shouldBe false
    val dSet2 = diff(s1, s1)
    contains(dSet2, 1) shouldBe false
  }

  "filter" should "return subset of s for which p holds true" in {
    val fSet = filter(s1, x => x < 2)
    contains(fSet, 1) shouldBe true
    val fSet2 = filter(s2, x => x < 2)
    contains(fSet2, 2) shouldBe false
  }

  "forall" should "return whether all bounded integers within s satisfy p" in {
    forall(s1, x => x < 2) shouldBe true
    forall(s2, x => x < 2) shouldBe false
  }

  "exists" should "return whether there exists an integer within s that satisfies p" in {
    val uSet = union(s1, s2)
    exists(uSet, x => x < 2) shouldBe true
    exists(uSet, x => x > 2) shouldBe false
  }

  "map" should "transform a set by applying f to each element" in {
    val mSet = map(s1, x => x + 1)
    contains(mSet, 1) shouldBe false
    contains(mSet, 2) shouldBe true
  }
}
