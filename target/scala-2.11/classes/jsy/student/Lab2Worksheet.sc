/*
 * CSCI 3155: Lab 2 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab2.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab2.Parser.parse

// Imports the ast nodes
import jsy.lab2.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab2._

// Call the JavaScripty parser (from the provided library) on a string
val negFourAST = parse("-4")


parse("-5 + 3 + 2")
sealed abstract class SearchTree
case object Empty extends SearchTree
case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

def sum(t: SearchTree) : Int = t match {
  case Empty => 0
  case Node(l,d,r) => d + sum(l) + sum(r)
}


case Binary(bop, e1, e2) bop match {
  case Plus => N(toNumber(eval(env, e1)) + toNumber(eval(env, e2))) // 2 + "true" = 2true?
  case Minus => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))
  case Times => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))
  case Div => N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))
}























println(B(1 < 2))
println("asasdf".toDouble)
println("123".toDouble)



// Evaluate that JavaScripty expression.
//eval(negFourAST)

// For convenience, we also have an eval function that takes a string,
// which calls the parser and then delegates to your eval function.
//eval("undefined + 1")

