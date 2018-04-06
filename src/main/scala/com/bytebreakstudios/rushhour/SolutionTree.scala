package com.bytebreakstudios.rushhour

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

class SolutionTree(val board:Board) extends Ordered[SolutionTree] {
  private var depth:Int = 0
  var parent:SolutionTree = _
  val children:ListBuffer[SolutionTree] = new ListBuffer[SolutionTree]()
  lazy val isSolution:Boolean = board.isSolved
  lazy val spacesUntilSolved:Int = board.spacesUntilSolved

  def readDepth():Int = depth

  def addChild(child:SolutionTree):Boolean = {
    root.find(child) match {
      case Some(found) =>
        if (found != this && depth + 1 < found.depth) {
          //          println(s"Found, better! (${depth + 1} < ${found.depth})${if(found.isSolution) "*" else ""}:\n${child.board}")
          if (found.parent != null) found.parent.children -= found
          found.parent = this
          found.recursivelySetDepth(depth + 1)
          children += found
          false
        } else {
          //          println(s"Found, but not good enough(${depth + 1} >= ${found.depth})${if(found.isSolution) "*" else ""}:\n${child.board}")
          //          children += found // TODO: this causes circular references... damnit
          false
        }
      case None =>
        children += child
        child.parent = this
        child.depth = depth + 1
        //        println(s"New (${child.depth}${if(child.isSolution) "*" else ""}):\n${child.board}")
        true
    }
  }

  private def findRoot():SolutionTree = if (parent == null) this else parent.findRoot()
  lazy val root:SolutionTree = findRoot()

  private def recursivelySetDepth(depth:Int):Unit = {
    this.depth = depth
    children.foreach(c => c.recursivelySetDepth(depth + 1))
  }

  private def find(child:SolutionTree):Option[SolutionTree] = {
    if (eq(child)) Option(this)
    else {
      var found: Option[SolutionTree] = None
      breakable {
        for (i <- children.indices) {
          found = children(i).find(child)
          if (found.nonEmpty) break
        }
      }
      found
    }
  }

  def ancestry():List[SolutionTree] = this :: (if (parent == null) Nil else parent.ancestry())

  def eq(other:SolutionTree):Boolean = board == other.board

  override def toString: String = s"${" " * depth}$depth${if (isSolution) "*" else ""}\n$board${if (children.nonEmpty) "\n" else ""}${children.mkString("\n")}"

  private def aStarHeuristicValue():Int = spacesUntilSolved + (depth * 3)

  override def compare(that: SolutionTree): Int = that.aStarHeuristicValue() - this.aStarHeuristicValue()
}
