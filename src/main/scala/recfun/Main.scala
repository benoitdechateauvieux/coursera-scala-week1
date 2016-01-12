package recfun

import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceRec(nbOpen: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) (nbOpen == 0)
      else if (chars.head == '(') balanceRec(nbOpen + 1, chars.tail)
      else if (chars.head == ')')
        if (nbOpen == 0) false
        else balanceRec(nbOpen - 1, chars.tail)
      else
        balanceRec(nbOpen, chars.tail)
    balanceRec(0, chars)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeRec(change: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (change==coins.head) 1 + countChangeRec(change, coins.tail)
      else if (change>coins.head) countChangeRec(change-coins.head, coins) + countChangeRec(change, coins.tail)
      else countChangeRec(change, coins.tail) //if (change<coins.head)
    }
    if (money == 0) 0
    else countChangeRec(money, (coins.sorted).reverse)
  }
}
