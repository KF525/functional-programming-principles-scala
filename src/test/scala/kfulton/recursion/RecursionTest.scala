package kfulton.recursion

import org.scalatest.{FlatSpec, Matchers}

class RecursionTest extends FlatSpec with Matchers {
  val recursion = new Recursion

  "pascal" should "return correct value in triangle" in {
    recursion.pascal(0,0) shouldBe 1
    recursion.pascal(0,2) shouldBe 1
    recursion.pascal(4,4) shouldBe 1
    recursion.pascal(4,5) shouldBe 5
    recursion.pascal(3,5) shouldBe 10
    recursion.pascal(1,2) shouldBe 2
    recursion.pascal(1,3) shouldBe 3
  }

  it should "handle invalid inputs" in {
    recursion.pascal(10,3) shouldBe -1
  }

  "balance" should "return true if parentheses are balanced" in {
    val chars1 = List('(', '(', ')', ')')
    val chars2 = List('(','i','f','(','z','e','r','o','?', 'x',')', 'm','a','x','(','/','1','x',')',')')
    recursion.balance(chars1) shouldBe true
    recursion.balance(chars2) shouldBe true
  }

  it should "return false if parentheses are not balanced" in {
    val chars = List('(',')',')','(')
    recursion.balance(chars) shouldBe false
  }

  it should "return true if the list is empty" in {
    val empty = List()
    recursion.balance(empty) shouldBe true
  }

  "countChange" should "return the number of change combinations" in {
    recursion.countChange(4, List(2,1)) shouldBe 3
    recursion.countChange(4, List(3,2,1)) shouldBe 4
    recursion.countChange(6, List(3,2,1)) shouldBe 7
  }

  it should "handle different coin orders" in {
    recursion.countChange(4, List(1,2, 3)) shouldBe 4
  }

  it should "handle the case where no monetary amount is given" in {
    pending
  }

  it should "handle the case where no coins are given" in {
    recursion.countChange(4, List()) shouldBe 0
  }
}
