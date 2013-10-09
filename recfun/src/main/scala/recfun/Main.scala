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
  def pascal(c: Int, r: Int): Int = {
    if (r == 0) 1
    else if (r == 1) 1
    else if (c == 0) 1
    else if (c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def innerBal(sub: List[Char], open: Int): Boolean = {
      if      (sub.isEmpty && open == 0) true
      else if (sub.isEmpty && open != 0) false
      else if (open < 0) false
      else if (sub.head == '(') innerBal(sub.tail, open + 1)
      else if (sub.head == ')') innerBal(sub.tail, open - 1)
      else    innerBal(sub.tail, open)
    }
    
    innerBal(chars, 0)
    
  }

  /**
   * Exercise 3, money is total to make, list is coins to do it with
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    //If money is less than 0 we have used too many coins
    if      (money < 0) 0
    
    //If money is exactly 0, we have found a valid solution
    else if (money == 0) 1
    
    //If money is greater than 0, but we have run out of coins to use, we've failed
    else if (money > 0 && coins.isEmpty) 0
    
    //Otherwise, recursively explore possibilities through a tree
    else    countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}


