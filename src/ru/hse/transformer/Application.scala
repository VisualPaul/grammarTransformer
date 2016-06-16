package ru.hse.transformer

import scala.tools.nsc.io.File

import org.json4s.DefaultFormats
import org.json4s.native.Serialization

import scala.io.Source

sealed abstract class InputState {
}
case object NoGrammarYet extends InputState
case class GrammarRead(grammar: Grammar) extends InputState
case class ReadingGrammar(grammar: List[Rule]) extends InputState
case object Exit extends InputState
case class LoadAwaitingName(previousState: InputState) extends InputState
case class SaveAwaitingName(previousState: InputState) extends InputState

object Application {
  def main(args: Array[String]) : Unit = {
    //Grammar.printToConsole(Grammar.readFromConsole.leftLinear2rightLinear())
    val lines = Source.stdin.getLines
    val startingSymbol = Symbol(terminal = false, "S")
    implicit val format = DefaultFormats
    Stream.iterate(NoGrammarYet: InputState)(state => {
      val line = lines.next()
      (state, line) match {
        case (Exit, _) => Exit
        case (LoadAwaitingName(st), "") =>
          println("cancelled")
          st
        case (SaveAwaitingName(st), "") =>
          println("cancelled")
          st
        case (LoadAwaitingName(st), name) =>
          try {
            GrammarRead(Serialization.read[Grammar](Source.fromFile(name).mkString))
          } catch {
            case e: Exception =>
              println("error occurred")
              st
          }
        case (SaveAwaitingName(GrammarRead(gr)), name) =>
          try {
            File(name).writeAll(Serialization.write(gr))
          } catch {
            case e: Exception =>
              println("error occurred")
          }
          GrammarRead(gr)
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
        case (st, "load") =>
          print("Enter file name: ")
          LoadAwaitingName(st)
        case (GrammarRead(g), "save") =>
          print("Enter file name: ")
          SaveAwaitingName(GrammarRead(g))
        case (st, cmd) =>
          println("incorrect command")
          st
      }
    }).takeWhile(_ != Exit).foreach(x=> ())

  }
}