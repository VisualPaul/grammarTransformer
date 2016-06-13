package ru.hse.transformer

import scala.io.Source

sealed abstract class RegularCase
case object LefttLinear extends RegularCase
case object RightLinear extends RegularCase
case class Term(terminal: Boolean, id: String) {
  def derivative = Term(terminal, id + "'")
}
case class Rule(lhs: List[Term], rhs: List[Term])

trait Grammar {
  val startingTerm : Term
  val rules: List[Rule]
  def isContextFree = rules.forall(r => r.lhs.size == 1 && !r.lhs.head.terminal)
  def isRegular(regularCase: RegularCase) = isContextFree && rules.forall(rule => {
    val r = if (regularCase == RightLinear) rule.rhs else rule.rhs.reverse
    r match {
      case List() => true
      case List(x) => x.terminal
      case List(x, y) => x.terminal && !y.terminal
      case _ => false
    }
  })
  def isRegular = isContextFree && rules.forall(r => r.rhs.count(x=> !x.terminal) <= 1)
  def leftLinear2rightLinear(newStart : Term = startingTerm.derivative) : Grammar = {
    assert(isRegular(LefttLinear))
    val needNewStart = rules.exists(r => r.rhs.contains(startingTerm))
    val oldStartingTerm = startingTerm
    val oldRules = rules
    val old = this
    new Grammar {
      override val startingTerm : Term = if (needNewStart)
        Term(oldStartingTerm.terminal, oldStartingTerm.id + "'")
      else oldStartingTerm
      override val rules: List[Rule] = oldRules.map {
        case Rule(List(`oldStartingTerm`), List(x)) =>
          Rule(List(startingTerm), List(x))
        case Rule(List(`oldStartingTerm`), List(x, y)) =>
          Rule(List(x), List(y))
        case Rule(List(x), List(y)) =>
          Rule(List(startingTerm), List(Term(y.terminal, y.id), x))
        case Rule(List(x), List(y, z)) =>
          Rule(List(y), List(z, x))
      } ++ (if (needNewStart) List(Rule(List(startingTerm), List(oldStartingTerm))) else List())
    }
  }
}

object Grammar {
  def readFromConsole : Grammar = {
    val toTerm = (x: Char) => Term(x.isLower, x.toString)
    val readRules = Source.stdin.getLines.map(ln => {
      var arr = ln.split("->")
      assert(arr.length <= 2 && arr.nonEmpty)
      if (arr.length == 1)
        arr = Array(arr(0), "")
      Rule(arr(0).map(toTerm).toList, arr(1).map(toTerm).toList)
    }).toList
    new Grammar {
      override val startingTerm: Term = toTerm('S')
      override val rules: List[Rule] = readRules
    }
  }
  def printToConsole(grammar: Grammar): Unit = {
    for (rule <- grammar.rules) {
      val line = rule.lhs.map(_.id).mkString + "->" + rule.rhs.map(_.id).mkString
      println(line)
    }
  }
}