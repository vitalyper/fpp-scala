package recfun
import common._
import scala.collection.immutable.TreeSet

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

    def buildRow(xs: List[Int]): List[Int] = xs match {
      case x :: Nil => List(x, x)
      case _ => 1 +: xs.sliding(2,1).toList.map(x => x.sum) :+ 1
  }

    def findAt(xs: List[Int], ar: Int): Int = 
      if (r == ar) xs(c) else findAt(buildRow(xs), ar + 1)

    findAt(List(1), 0)
    }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def nextCnt(c: Char, i: Int): Int = c match {
      case '(' => i + 1
      case ')' => i - 1
      case _ => i
    }

    def balanceIter(xs: List[Char], i: Int): Boolean = xs match {
      case Nil => true
      case x :: Nil => nextCnt(x, i) == 0
      case x :: xs => {
        val i2 = nextCnt(x, i)
        if (i2 < 0) false else balanceIter(xs, i2)
      }
    }
    
    balanceIter(chars, 0)
  }



  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, xs: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || xs.length == 0) 0
      else {
        (countChangeIter(money, xs.tail)
        +
        countChangeIter(money - xs.head, xs))
      }
    }

    countChangeIter(money, (TreeSet[Int]() ++ coins).toList)
  }
}
