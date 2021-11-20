@main def Main : Unit = {

  def calculatorPolish(str: String): Int = {
    //println(str)
    def calc(i: Int): (Int, Int) = {
      //var sum:Int = calc(i+1)._1 + calc(calc(i+1)._2+1)._1
      //var quotient:Int = calc(i+1)._1 * calc(calc(i+1)._2+1)._1
      //if (str.length < i)  (calc(i-1)._1,i-1)
      if (str.charAt(i) == '*') {
        /*println("*")
        println(i)
        println("--------------------------")*/
        if (!(str.charAt(i+1)).isDigit) {
          if (str.charAt(i) == '*') (calc(i+1)._1 * calc(calc(i+1)._2+1)._1,calc(calc(i+1)._2+1)._2)
          else (calc(i+1)._1 + calc(calc(i+1)._2+1)._1,calc(calc(i+1)._2+1)._2)
        }
        else (calc(i+1)._1*calc(i+2)._1,calc(i+2)._2)
      }
      else if (str.charAt(i).isDigit) {
        /*println((i,"i"))*/
        (str.charAt(i).toString.toInt,i)
      }
      else if (str.charAt(i) == '+') {

        /*println("+")
        println(i)
        println("--------------------------")*/
        if (!(str.charAt(i+1)).isDigit){
          if (str.charAt(i) == '*') (calc(i+1)._1 * calc(calc(i+1)._2+1)._1,calc(calc(i+1)._2+1)._2)
          else (calc(i+1)._1 + calc(calc(i+1)._2+1)._1,calc(calc(i+1)._2+1)._2)
        }
        else (calc(i+1)._1+calc(i+2)._1,calc(i+2)._2)
      }
      else (calc(str.charAt(i))._1,i)
    }



    if (str.length == 1) str.charAt(0).toString.toInt
    else calc(0)._1
  }
  //test("calculatorPolish", calculatorPolish _, "str")
  println("""RESULTSvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvRESULTS
  """)
  println("calculatorPolish Result: " + calculatorPolish("++333"))

  def calculatorParens(str: String): Int = {
    def calc(i: Int): (Int, Int) = {
      //println(("i",i))
      //println(("value at index",str.charAt(i)))
      if (str.length == 3) (str.charAt(1).toString.toInt,1)
      else if (str.charAt(i) == '(') {
        if (str.charAt(calc(i+1)._2+1) =='+') {
          (calc(i+1)._1+calc(calc(i+1)._2+2)._1,calc(calc(i+1)._2+2)._2+1)
        }
        else (calc(i+1)._1*calc(calc(i+1)._2+2)._1,calc(calc(i+1)._2+2)._2+1)
      }
      else (str.charAt(i).toString.toInt,i)
     }
    calc(0)._1
  }

  println("""
       ----------------------------------------
       """)
  println("Results calculatorParens: " + calculatorParens("((3*5)+3)+4"))
//test("calculatorParens", calculatorParens _, "str")

  def calculatorStandard(str: String): Int = {
    def calc2(i: Int): (Int, Int) = {
      println(("calc2",i))
      // parse one or more level-1 expressions separated by +'s
      if (str.length == 2) (calc0(i)._1,i)
      else if (str.length == 4) (calc0(i+1)._1,i+1)
      else if (str.charAt(i+1) == '!') (0,0)
      else if (str.charAt(i+1) == '+') (calc1(i)._1+calc1(i+2)._1,calc1(i+2)._2)
      else if (str.charAt(i+1) == '*') (calc1(i)._1,calc1(i)._2)
      else if (str.charAt(i) == '(') (calc2(i+1)._1,calc2(i+1)._2)
      else (calc2(i+1)._1,calc2(i+1)._2)
    }
    def calc1(i: Int): (Int, Int) = {
      println(("calc1",i))
      // parse one or more level-0 expressions separated by *'s
      //if (str.charAt(i-1).isDigit && str.charAt(i+1).isDigit)
      if (str.length == 2) (calc0(i)._1,i)
      else if (str.length == 4) (calc0(i+1)._1,i+1)
      else if (str.charAt(i+1) == '!') (0,0)
      else if (str.charAt(i+1) == '*') {println(calc0(i)._1*calc0(i+2)._1);(calc0(i)._1*calc0(i+2)._1,calc1(i+2)._2)}
      else if (str.charAt(i+1) == '+') (calc1(i)._1,calc1(i)._2)
      else if (str.charAt(i) == '(') (calc0(i)._1,calc0(i)._2)
      else (calc2(i+1)._1,calc2(i+1)._2)
    }
    def calc0(i: Int): (Int, Int) = {
      println(("calc0",i))
      // parse a single digit, or a level-2 expression in parentheses
      if (str.length == 2) (str.charAt(i).toString.toInt,i) //case with only one number
      else if (str.length == 4) (str.charAt(i).toString.toInt,i)
      else if (str.charAt(i) == '(') (calc2(i+1)._1,calc2(i+1)._2)
      else if (str.charAt(i).isDigit) (str.charAt(i).toString.toInt,i+1)
      else { //phrase in parentheses
        if (str.charAt(i+1) == '+') (calc2(i)._1,i+1)
        else  (calc1(i)._1,i+1)
      }
    }
    calc2(0)._1
  }
  println("""
       ----------------------------------------""")
  println("Results calculatorStandard: " + calculatorStandard("3*3"))
  //test("calculatorStandard", calculatorStandard _, "str")
  println("""
RESULTS^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^RESULTS""")
}
