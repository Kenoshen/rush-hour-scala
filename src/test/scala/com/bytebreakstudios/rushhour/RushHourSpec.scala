package com.bytebreakstudios.rushhour

import org.scalatest.WordSpec

import scala.util.Random

class RushHourSpec extends WordSpec {
  "Board" should {
    "get all valid moves right and down" in {
      val board = Board(Car.leadCarPreset.at(0, 2) :: Car.verticalTruckPreset.at(5, 0).withId("1") :: Nil)
      val a = board.find("0").get
      val b = board.find("1").get
      assertResult(List((3, 2)).sortBy(_._1))(board.validMoves(a).sortBy(_._1))
      assertResult(List((5, 3)).sortBy(_._2))(board.validMoves(b).sortBy(_._2))
    }

    "get all valid moves left and up" in {
      val board = Board(Car.leadCarPreset.at(4, 2) :: Car.verticalTruckPreset.at(0, 3).withId("1") :: Nil)
      val a = board.find("0").get
      val b = board.find("1").get
      assertResult(List((0, 2)).sortBy(_._1))(board.validMoves(a).sortBy(_._1))
      assertResult(List((0, 0)).sortBy(_._2))(board.validMoves(b).sortBy(_._2))
    }

    "get all valid moves right, left, down and up" in {
      val board = Board(Car.leadCarPreset.at(2, 2) :: Car.verticalTruckPreset.at(0, 2).withId("1") :: Nil)
      val a = board.find("0").get
      val b = board.find("1").get
      assertResult(List((4, 2), (1, 2)).sortBy(_._1))(board.validMoves(a).sortBy(_._1))
      assertResult(List((0, 0), (0, 3)).sortBy(_._2))(board.validMoves(b).sortBy(_._2))
    }

    "find shortest solution to test board 1" in {
      val a = Board(Car.leadCarPreset.at(0, 2) :: Car.verticalTruckPreset.at(5, 0).withId("1") :: Nil)
      val solution = Board.solveBoard(a)
      println(solution)
      assertResult(2)(solution.steps)
    }

    "find shortest solution to test board 2" in {
      val a = Board(Car.leadCarPreset.at(0, 2) :: Car.verticalTruckPreset.at(2, 0).withId("1") :: Car.verticalTruckPreset.at(5, 0).withId("2") :: Nil)
      val solution = Board.solveBoard(a)
      println(solution)
      assertResult(3)(solution.steps)
    }

    "find no solution to test board 3" in {
      val a = Board(Car.leadCarPreset.at(0, 2) :: Car.verticalTruckPreset.at(5, 0).withId("1") :: Car.verticalTruckPreset.at(5, 3).withId("2") :: Nil)
      val solution = Board.solveBoard(a)
      println(solution)
      assertResult(-1)(solution.steps)
    }

    "find shortest solution to test board 4" in {
      val a = Board(Car.leadCarPreset.at(3, 2) ::
        Car.verticalTruckPreset.at(2, 0).withId("1") ::
        Car.verticalTruckPreset.at(5, 0).withId("2") ::
        Car.verticalTruckPreset.at(3, 3).withId("3") ::
        Car.verticalCarPreset.at(0, 4).withId("4") ::
        Car.horizontalCarPreset.at(3, 0).withId("5") ::
        Car.horizontalCarPreset.at(4, 3).withId("6") ::
        Car.horizontalCarPreset.at(1, 4).withId("7") ::
        Car.horizontalCarPreset.at(4, 5).withId("8") :: Nil)
      val solution = Board.solveBoard(a)
      println(solution)
      assertResult(33)(solution.steps)
    }

    "find shortest solution to test board 5" in {
      val a = Board(Car.leadCarPreset.at(2, 2) ::
        Car.horizontalTruckPreset.at(0, 0).withId("1") ::
        Car.verticalTruckPreset.at(4, 0).withId("2") ::
        Car.verticalTruckPreset.at(5, 0).withId("3") ::
        Car.verticalCarPreset.at(0, 1).withId("4") ::
        Car.verticalCarPreset.at(3, 0).withId("5") ::
        Car.verticalCarPreset.at(2, 3).withId("6") ::
        Car.verticalCarPreset.at(1, 4).withId("7") ::
        Car.horizontalCarPreset.at(1, 1).withId("8") ::
        Car.horizontalCarPreset.at(0, 3).withId("9") ::
        Car.horizontalCarPreset.at(2, 5).withId("a") ::
        Car.horizontalCarPreset.at(4, 4).withId("b") ::
        Car.horizontalCarPreset.at(4, 5).withId("c") :: Nil)
      val solution = Board.solveBoard(a)
      println(solution)
      assertResult(50)(solution.steps)
    }

    "find solution to any random board" in {
      val rnd = new Random()
      val a = Board.randomBoard(4 + rnd.nextInt(10))
      val steps = Board.solveBoard(a).steps
      if (steps < 0) assertResult(-1)(steps)
      else assert(steps != 0, "steps must not equal 0")
    }

    "find at least one board that takes more than 5 steps" in {
      var next:BoardMeta = null
      val rnd = new Random()
      var attempts = 0
      val maxAttempts = 25
      while ((next == null || next.steps < 5) && attempts < maxAttempts) {
        next = Board.solveBoard(Board.randomBoard(8 + rnd.nextInt(6)))
        println(next)
        attempts += 1
      }
      assert(attempts != 25, "hit the max attempts to find a board with more than 5 steps")
      assert(next != null, "next was null for some odd reason")
      assert(next.steps >= 5, "the found board has less than 5 steps to solution")
    }

    "boards with cars on the lead car track that are in front of the lead car are automatically invalid" in {
      /*
        ++++++
        ++++++
        00+22+
        ++++++
        ++++++
        ++++++
       */

      val a = Board(Car.leadCarPreset.at(0, 2) ::
        Car.horizontalCarPreset.at(3, 2).withId("1") :: Nil)
      intercept[RuntimeException]{
        Board.solveBoard(a)
      }
    }

    "use a 7 by 7 board to generate an INSANE board" in {
      cancel()
      val board = Board.randomBoard(BoardDifficulty.INSANE)
      assert(board.nonEmpty)
    }
  }
}
