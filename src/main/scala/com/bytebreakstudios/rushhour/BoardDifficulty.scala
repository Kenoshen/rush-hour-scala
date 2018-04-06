package com.bytebreakstudios.rushhour

case class BoardDifficulty(label:String, minSteps:Int, maxSteps:Int)

object BoardDifficulty {
  val INVALID = BoardDifficulty("INVALID", -1000, 2)
  val EASY = BoardDifficulty("EASY", 3, 5)
  val MEDIUM = BoardDifficulty("MEDIUM", 6, 9)
  val HARD = BoardDifficulty("HARD", 10, 13)
  val INSANE = BoardDifficulty("INSANE", 13, 1000)

  val ALL = List(INVALID, EASY, MEDIUM, HARD, INSANE)

  def apply(steps:Int):String = ALL.find(d => steps >= d.minSteps && steps <= d.maxSteps).get.label

  def apply(label:String):Option[BoardDifficulty] = ALL.find(_.label.equalsIgnoreCase(label))
}