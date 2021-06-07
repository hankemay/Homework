/**
  *
  * Description:
  * Calc
  *
  * @author hmxiao
  * @version 1.0
  */

import scala.collection.mutable.Stack
object SimpleCalc {

  def calculate(input: String): Int = {
    var numStack = new Stack[Int]()
    var newInput = input + "+"

    var opSet = Set('+', '-', '*', '/')

    var preOp = '+'
    var num = 0


    for (c <- newInput) {
      if (c >= '0' && c <= '9') {
        num = num * 10 + c - '0'
      } else if (opSet.contains(c)) {
        preOp match {
          case '+' => {
            numStack.push(num)
          }
          case '-' => {
            numStack.push(-num)
          }
          case '*' => {
            if (numStack.size < 0) incorrectInputError
            numStack.push(numStack.pop * num)
          }
          case '/' => {
            if (numStack.size < 0) incorrectInputError
            numStack.push(numStack.pop / num)
          }
        }
        num = 0
        preOp = c
      } else incorrectInputError
    }

    numStack.sum
  }

  def incorrectInputError() : Unit = {
    throw new Exception("incorrect input")
  }

  def main(args: Array[String]): Unit = {
    val strA = "10+2*3+1-7"
    println(calculate(strA))
    assert(calculate(strA) == 10)

    val strB = "10+2*3/6+5*6-7+8-9"
    assert(calculate(strB) == 33)

    val strC = "+-*18+2/2"
    assert(calculate(strC) == 1)

    val strE = ""
    assert(calculate(strE) == 0)

    val strD = "10+2*!8"
    assert(calculate(strD) == 10) //throw exception

//    println(calculate(strC))
//    calculate(strC)

  }

}
