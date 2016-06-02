package recfun

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Money Conversion")
    val cm = List(1,2,3)
    println(countChange(8,cm))

    val cc = "(if (zero? x) max (/ 1 x))"
    println(balance(cc.toList))

    val cc1 = "())("
    println(balance(cc1.toList))

  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {

    if ((c == 0) || (r==c)) 1 else pascal(c-1,r-1)+pascal(c,r-1)

  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def fun2(ch: List[Char], n: Int): Boolean = {
         if (ch.isEmpty) n==0 else {
           if (n < 0) false else {
             if (ch.head == '(') fun2(ch.tail, n + 1) else {
               if (ch.head == ')') fun2(ch.tail, n - 1) else fun2(ch.tail, n)
             }
           }
         }
      }

      fun2(chars,0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

    def ways(m:Int, ca:List[Int]): Int = {
      if (ca.isEmpty || m < 0) 0 else { if(m==0) 1 else ways(m - ca.head,ca) + ways(m, ca.tail) }
    }

    ways(money,coins)

    }

  }
