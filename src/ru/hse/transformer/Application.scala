package ru.hse.transformer

object Application {
  def main(args: Array[String]) = {
    Grammar.printToConsole(Grammar.readFromConsole.leftLinear2rightLinear())
  }
}