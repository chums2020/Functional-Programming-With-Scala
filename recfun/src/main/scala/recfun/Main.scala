package recfun

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
    if (c == 0 || c == r) 1
    else if (r < 0) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  // intermediate parameter: start last parenthesis is closing parenthesis
  def balance(chars: List[Char]): Boolean = {

    def balanced(chars: List[Char], start: Int): Boolean = {
      // start == 0 => balanced
      if (chars.isEmpty) start == 0
      else if (chars.head.toString == "(") balanced(chars.tail, start + 1)
      // if open > 0 is false, then balanced will return false
      else if (chars.head.toString == ")") balanced(chars.tail, start - 1) && start > 0
      else balanced(chars.tail, start)
    }

    balanced(chars, 0) // starts open at 0
  }

  //println("(just an example)".toList)
  /**
    * Exercise 3
    */

  def countChange(money: Int, coins: List[Int]): Int = {
    def startCountChange(money: Int, coins: List[Int], count: Int): Int = {
      if (money < 0) count
      else if (coins.isEmpty && money == 0) count + 1
      else if (coins.isEmpty && money != 0) count
      else startCountChange(money, coins.tail, count) + startCountChange(money - coins.head, coins, count)
    }

    startCountChange(money, coins, 0)
  }
}