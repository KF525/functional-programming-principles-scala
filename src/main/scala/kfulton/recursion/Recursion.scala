package kfulton.recursion

class Recursion {
  def pascal(col: Int, row: Int): Int = (col,row) match {
    case (0, _) => 1
    case end if col.equals(row) => 1
    case invalid if col > row => -1
    case _ => pascalHelper(col, row)
  }

  private def pascalHelper(col: Int, row: Int): Int = (col, row) match {
    case (c, r) if r.equals(c) => 1
    case (0, _) => 1
    case (c, r) => pascalHelper(c-1, r-1) + pascalHelper(c, r-1)
  }

  def balance(chars: List[Char]): Boolean = balanceHelper(chars)

  private def balanceHelper(chars: List[Char], counter: Int = 0): Boolean = (chars, counter) match {
    case (_, count) if count < 0 => false
    case (Nil, count) if count.equals(0) => true
    case (Nil, _) => false
    case (h::t, count) if h.equals('(') => balanceHelper(t, count + 1)
    case (h::t, count) if h.equals(')') => balanceHelper(t, count - 1)
    case (h::t, count) => balanceHelper(t, count)
  }

  def countChange(money: Int, coins: List[Int]): Int =
    if (money.equals(0)) 0 else countChangeHelper(money, coins)

  def countChangeHelper(money: Int, coins: List[Int]): Int = {
    if (money.equals(0)) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChangeHelper(money - coins.head, coins) + countChangeHelper(money, coins.tail)
  }
}