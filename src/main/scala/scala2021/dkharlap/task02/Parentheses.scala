package scala2021.dkharlap.task02

import scala.annotation.tailrec

object Parentheses {
  def main(args: Array[String]): Unit = {
    val inputValue1 = List[Char]('i','f','(','(','2','+','x',')','*','(','3','-','y',')','=','=','3',')')
    val isBalanced1 = checkBalance(inputValue1)
    println("Parentheses in inputValue1 are balanced: " + isBalanced1)

    val inputValue2 = List[Char]('Я',' ','с','к','а','з','а','л',' ','е','м','у',' ','(','э','т','о',' ','е','щ','е',' ','(','н','е',')',
                                          'с','д','е','л','а','н','о',')','.',' ','(','Н','о',' ','о','н',' ','н','е',' ','п','о','с','л','у','ш','а','л',')')
    val isBalanced2 = checkBalance(inputValue2)
    println("Parentheses in inputValue2 are balanced: " + isBalanced2)

    val inputValue3 = List[Char](':','-',')')
    val isBalanced3 = checkBalance(inputValue3)
    println("Parentheses in inputValue3 are balanced: " + isBalanced3)

    val inputValue4 = List[Char]('(',')',')','(')
    val isBalanced4 = checkBalance(inputValue4)
    println("Parentheses in inputValue4 are balanced: " + isBalanced4)
  }

  def checkBalance(inputValue: List[Char]): Boolean = {
    @tailrec
    def balance(chars: List[Char], openParenthesesNumber: Int): Boolean = {
      if (chars.isEmpty) {
        openParenthesesNumber == 0
      } else {
        val headChar = chars.head
        val openParentheses =
          if (headChar == '(') {
            openParenthesesNumber + 1
          } else if (headChar == ')') {
            openParenthesesNumber - 1
          } else {
            openParenthesesNumber
          }

        if (openParentheses >= 0) {
          balance(chars.tail, openParentheses)
        } else {
          false
        }
      }
    }

    balance(inputValue, 0)
  }
}
