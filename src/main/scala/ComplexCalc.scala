import scala.collection.mutable.Stack

/**
  * Created at 2021-06-05 11:54
  *
  * Description:
  * ComplexCalc
  *
  * @author hmxiao
  * @version 1.0
  */
object ComplexCalc {
  val opSet = Set('+', '-', '*', '/')

  def calculate(input: String): Int = {
    val numStack = new Stack[Int]()
    val newInput = input + "+"

    var preOp = '+'
    var num = 0
    var i = 0
    var flag = 0


    while (i < newInput.size) {
      val c = newInput(i)

      //check validate case
      validate(flag, c)

      if (c >= '0' && c <= '9') {
        num = num * 10 + c - '0'
        flag = 1
      } else if (opSet.contains(c)) {
        preOp match {
          case '+' => {
             numStack.push(num)
          }
          case '-' => {
             numStack.push(-num)
          }
          case '*' => {
            if ( numStack.size < 0) incorrectInputError
             numStack.push((numStack.pop * num))
          }
          case '/' => {
            if ( numStack.size < 0) incorrectInputError
             numStack.push((numStack.pop / num))
          }
        }
        num = 0
        preOp = c

        flag = 2
      } else if (c == '(') {
        val j = newInput.indexOf(')', i + 1)
        if (j == -1) incorrectInputError()

        val value = calculate(newInput.substring(i + 1, j))
        num = value

        i = j

        flag = 3
      } else incorrectInputError()

      i += 1


    }


    numStack.sum
  }

  def incorrectInputError() : Unit = {
    throw new Exception("incorrect input")
  }

  def validate(flag: Int, c: Char): Unit = {
    //check validate case
    flag match {
      //the beginning
      case 0 => {
        if ((c < '0' || c > '9') && (c != '(')) incorrectInputError()
      }
      //the pre is num, current is not num and op
      case 1 => {
//        println(c)
        if ((c < '0' || c > '9') && !opSet.contains(c)) incorrectInputError()
      }
      //the pre is op, current is not num and (
      case 2 => {
        if ((c < '0' || c > '9') && (c != '(')) incorrectInputError()
      }
      //the pre is (, current is not op
      case 3 => {
        if (!opSet.contains(c)) incorrectInputError()
    }

    }
  }

  def main(args: Array[String]): Unit = {
    println("Begin the unit test!")

    val strA = "10+2*(3+1-7)"
    assert(calculate(strA) == 4)

    val strB = "10+2*3/6+5*6-7+8-9"
    assert(calculate(strB) == 33)

    val strD = "(8*9)+(2-5)"
    assert(calculate(strD) == 69)

//    val strC = "+-*18+2/2"
//    assert(calculate(strC) == 1)
//
//    val strE = ""
//    assert(calculate(strE) == 0)

    //    println(calculate(strC))
    //    calculate(strC)

    println("The unit test passed")

  }
}
