package recfun

import com.sun.org.apache.xpath.internal.functions.FuncFalse

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    println("Balance Parentheses")
    print(s"${balance("()()()adhafjdb()da,f())(".toList)}")
    println()
    println("Count Change")
    val list = List(1, 2, 3, 4)
    print(s"${countChange(6, list)}")
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
      if(c == 0 || c == r) 1
      else if(c < 0 || c > r) 0
      else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(acc: Int, chars:List[Char]): Boolean ={
      if (acc < 0) false
      else if (chars.isEmpty) acc == 0
      else if (chars.head == '(') loop(acc + 1, chars.tail)
      else if (chars.head == ')') loop(acc - 1, chars.tail)
      else loop(acc, chars.tail)
    }
    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(acc: Int, money: Int, coins: List[Int]): Int ={
      if (coins.isEmpty || money < 0) acc
      else if (money == 0) loop(acc + 1, money, Nil)
      else
        loop(acc, money - coins.head, coins) +
        loop(acc, money, coins.tail)
    }
    loop(0, money, coins)
  }
}
