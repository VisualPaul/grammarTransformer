package ru.hse.transformer

import scala.io.Source

sealed abstract class RegularCase
case object LeftLinear extends RegularCase
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
    val r = if (regularCase == LeftLinear) rule.rhs else rule.rhs.reverse
    r.isEmpty || r.tail.forall(_.terminal)
  })
  def isRegular = isContextFree && rules.forall(r => r.rhs.count(x=> !x.terminal) <= 1)
  def leftLinear2rightLinear(newStart : Term = startingTerm.derivative) : Grammar = {
    assert(isRegular(LeftLinear))
    val needNewStart = rules.exists(r => r.rhs.contains(startingTerm))
    val oldStartingTerm = startingTerm
    val oldRules = rules
    val old = this
    new Grammar {
      override val startingTerm : Term = if (needNewStart)
        Term(oldStartingTerm.terminal, oldStartingTerm.id + "'")
      else oldStartingTerm
      override val rules: List[Rule] = oldRules.map {
        case Rule(List(`oldStartingTerm`), l) =>
          if (l.isEmpty || l.head.terminal) // all terminals
            Rule(List(startingTerm), l)
          else
            Rule(List(l.head), l.tail)
        case Rule(List(x), l) =>
          if (l.isEmpty || l.head.terminal)
            Rule(List(startingTerm), l ++ List(x))
          else
            Rule(List(l.head), l.tail ++ List(x))
      }.distinct  ++ (if (needNewStart) List(Rule(List(startingTerm), List(oldStartingTerm))) else List())
    }
  }
}

object Grammar {
  private def readRuleSide(side : String) : List[Term] = {
    val tokensRaw = (side + " ").foldLeft((List[String](), List[Char]()))((l, c) => {
      val (terms, last) = l
      if (c.isSpaceChar) {
        if (last.nonEmpty) (last.reverse.mkString :: terms, List[Char]()) else l
      } else if (c.isLetter) {
        if (last.nonEmpty) (last.reverse.mkString :: terms, List[Char](c)) else (terms, List[Char](c))
      } else {
        assert(last.nonEmpty)
        (terms, c :: last)
      }
    })
    assert(tokensRaw._2.isEmpty)

    tokensRaw._1.reverse.map(str => {
      Term(str.exists(_.isLower), str)
    }).filter(_.id != "ε")
  }
  def readFromConsole : Grammar = {
    val toTerm = (x: Char) => Term(x.isLower, x.toString)
    val readRules = Source.stdin.getLines.map(ln => {
      val arr = ln.split("->").map(readRuleSide).toList
      assert(arr.length <= 2 && arr.nonEmpty)
      val (lhs, rhs) = arr match {
        case List(x) => (x, List())
        case List(x, y) => (x, y)
        case _ => throw new RuntimeException("unreachable code reached")
      }
      assert(lhs.exists(!_.terminal))
      Rule(lhs, rhs)
    }).toList
    new Grammar {
      override val startingTerm: Term = toTerm('S')
      override val rules: List[Rule] = readRules
    }
  }
  def printToConsole(grammar: Grammar): Unit = {
    for (rule <- grammar.rules) {
      val lhs = rule.lhs.map(_.id).mkString
      val rhs = if (rule.rhs.isEmpty) "ε" else rule.rhs.map(_.id).mkString
      println(lhs + "->" + rhs)
    }
  }
}