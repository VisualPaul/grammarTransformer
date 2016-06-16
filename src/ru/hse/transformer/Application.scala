package ru.hse.transformer

import scala.io.Source

sealed abstract class InputState {
}
case object NoGrammarYet extends InputState
case class GrammarRead(grammar: Grammar) extends InputState
case class ReadingGrammar(grammar: List[Rule]) extends InputState
case object Exit extends InputState
//case object LoadAwaitingName extends InputState
//case object SaveAwaitingName extends InputState

object Application {
  def main(args: Array[String]) : Unit = {
    //Grammar.printToConsole(Grammar.readFromConsole.leftLinear2rightLinear())
    val lines = Source.stdin.getLines
    val startingSymbol = Symbol(terminal = false, "S")
    Stream.iterate(NoGrammarYet: InputState)(state => {
      val line = lines.next()
      (state, line) match {
        case (Exit, _) => Exit
        case (ReadingGrammar(r), "end") => GrammarRead(Grammar(startingSymbol, r))
        case (ReadingGrammar(r), n) =>
          try {
            ReadingGrammar(Grammar.readRule(n) :: r)
          } catch {
            case e : Exception =>
              println("incorrect rule")
              ReadingGrammar(r)
          }
        case (_, "exit") => Exit
        case (_, "input") => ReadingGrammar(List())
        case (GrammarRead(x), "print") =>
          Grammar.printToConsole(x)
          GrammarRead(x)
        case (GrammarRead(x), "convert") =>
          try {
            GrammarRead(x.leftLinear2rightLinear())
          } catch {
            case e : Exception =>
              println("incorrect grammar")
              GrammarRead(x)
          }
        /*case (_, "load") =>
          LoadAwaitingName
        case (_, "save") =>
          SaveAwaitingName */
        case (st, cmd) =>
          println("incorrect command")
          st
      }
    }).takeWhile(_ != Exit).foreach(x=> ())

  }
}