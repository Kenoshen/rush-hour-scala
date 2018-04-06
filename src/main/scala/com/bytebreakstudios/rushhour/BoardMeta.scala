package com.bytebreakstudios.rushhour

case class BoardMeta(board:String, steps:Int, difficulty:String, solution:List[String], solutionMs:Long, maxFringe:Int, solutionFringe:Int, searchedNodes:Int) {
  override def toString: String = s"$metadata\n======\n$board\n======\n"
  lazy val metadata:String = s"$difficulty($steps)\n${solutionMs}ms\n[$solutionFringe:$maxFringe:$searchedNodes]"
}

object BoardMeta {
  def apply(board:Board, solutionTree: SolutionTree, solutionMs:Long, maxFringe:Int, solutionFringe:Int, searchedNodes:Int):BoardMeta = {
    val steps = solutionTree.readDepth()
    BoardMeta(board.toString, steps, BoardDifficulty(steps), solutionTree.ancestry().reverse.map(_.board.toString), solutionMs, maxFringe, solutionFringe, searchedNodes)
  }

  def invalid(board:Board, solutionMs:Long, maxFringe:Int, solutionFringe:Int, searchedNodes:Int):BoardMeta = BoardMeta(board.toString, -1, BoardDifficulty(-1), Nil, solutionMs, maxFringe, solutionFringe, searchedNodes)
}
