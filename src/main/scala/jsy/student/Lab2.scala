package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <D'Artagnan Wake>
   * 
   * Partner: <Sam Bennetts>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(b) => b match {
        case true => 1.0
        case false => 0.0
      }
      case S("") => 0
      case S(str) => try { str.toDouble }
        catch {
          case _: Throwable => Double.NaN // catches all exceptions
        }
      case Undefined => Double.NaN
      case _ => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => n match {
        case 0 => false
        case Double.NaN => false
        case _ => true
      }
      case S("") => false
      case S(_) => true
      case _ => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      case N(n) => if (n.isWhole) "%.0f" format n else n.toString
      case B(b) => b.toString
      case _ => throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */

      case N(n) => e
      case B(b) => e
      case S(s) => e
      case Undefined => Undefined

      /* Inductive Cases */

      case Binary(bop, e1, e2) => bop match {
        case Plus => (eval(env, e1), eval(env, e2)) match {
          case (S(s1), _) => S(toStr(S(s1)) + toStr(eval(env, e2)))
          case (_, S(s1)) => S(toStr(eval(env,e1)) + toStr(S(s1)))
          case (_, _) => N(toNumber(eval(env,e1)) + toNumber(eval(env, e2)))
        }
        case Minus => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))
        case Times => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))
        case Div => eval(env,e2) match {
          case N(0) => N(Double.PositiveInfinity)
          case _ => N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))
        }
        case And => (eval(env, e1), eval(env, e2)) match {
          case (B(b1), _) =>
            if(B(b1) == B(true)) eval(env, e2)
            else eval(env, e1)
          case (N(0),_) => N(0)
          case (_, B(b2)) => B(b2)
          case (Undefined, _) => Undefined
          case (_, Undefined) => Undefined
          case (_, _) => eval(env, e2)
        }

        case Or => (eval(env, e1), eval(env, e2)) match {
          case (B(b1), _) =>
            if(B(b1) == B(false)) eval(env, e2)
            else B(true)
          case (_, B(b2)) => eval(env, e1)
          case (Undefined, _) => Undefined
          case (_, Undefined) => Undefined
          case (_, _) => eval(env, e1)
        }
        case Eq => (eval(env, e1), eval(env, e2)) match {
          case (S(s1), S(s2)) => B(toStr(S(s1)) == toStr(S(s2)))
          case (_, _) => B(toNumber(eval(env, e1)) == toNumber(eval(env,e2)))
        }
        case Ne => B(!toBoolean(eval(Binary(Eq, e1, e2))))
        case Lt => (eval(env, e1), eval(env, e2)) match {
          case (S(s1), S(s2)) => B(toStr(S(s1)) < toStr(S(s2)))
          case (_, _) => B(toNumber(eval(env,e1)) < toNumber(eval(env, e2)))
        }
        case Le => (eval(env, e1), eval(env, e2)) match {
          case (S(s1), S(s2)) => B(toStr(S(s1)) <= toStr(S(s2)))
          case (_, _) => B(toNumber(eval(env, e1)) <= toNumber(eval(env, e2)))
        }
        case Gt => (eval(env, e1), eval(env, e2)) match {
          case (S(s1), S(s2)) => B(toStr(S(s1)) > toStr(S(s2)))
          case (_, _) => B(toNumber(eval(env, e1)) > toNumber(eval(env, e2)))
        }
        case Ge => (eval(env, e1), eval(env, e2)) match {
          case (S(s1), S(s2)) => B(toStr(S(s1)) >= toStr(S(s2)))
          case (_, _) => B(toNumber(eval(env, e1)) >= toNumber(eval(env, e2)))
        }
        case Seq => eval(env, e1);eval(env, e2)
      }

      case Unary(uop, e1) => uop match {
        case Neg => N(-1 * toNumber(eval(env, e1)))
        case Not => eval(env, e1) match {
          case B(b) => B(!b)
          case N(0) => B(true)
          case N(1) => B(false)
          case _ => B(false)
        }
      }
      case If(e1, e2, e3) => eval(env, e1) match {
        case (B(true)) => eval(env, e2)
        case (B(false)) => eval(env, e3)
        case (N(1)) => eval(env, e2)
        case (N(0)) => eval(env, e3)
      }
      case Var(x: String) => lookup(env, x)
      case ConstDecl(s: String, e1, e2) => eval(extend(env, s, eval(env, e1)), e2)



      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case _ => e
    }
  }



  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
