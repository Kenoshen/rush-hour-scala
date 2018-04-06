package com.bytebreakstudios.rushhour

import scala.collection.mutable
import scala.util.Random

case class Board(cars:List[Car], width:Int = 6, height:Int = 6) {
  override lazy val toString: String = (for {
    y <- 0 until width
    x <- 0 until height
  } yield {
    find(x, y).map(_.id).getOrElse("+")
  }).grouped(width).map(_.mkString("")).mkString("\n")

  lazy val spacesUntilSolved:Int = (width - 2) - find("0").get.x

  def find(x:Int, y:Int):Option[Car] = Board.find(x, y, cars)
  def find(id:String):Option[Car] = cars.find(_.id == id)
  def validMoves(car:Car):List[(Int, Int)] = Board.validMoves(car, this)
  lazy val isSolved:Boolean = find("0").exists(c => c.x == width - 2 && c.y == (height - 1) / 2)
  def moveCar(id:String, x:Int, y:Int):Board = copy(cars = cars.map{
    case c if c.id == id => c.at(x, y)
    case c => c
  })
  def moveCar(id:String, pos:(Int, Int)):Board = moveCar(id, pos._1, pos._2)
  lazy val isValid:Boolean = ((for {
    y <- 0 until width
    x <- 0 until height
  } yield {
    val allAtPos = Board.findAll(x, y, cars)
    allAtPos.isEmpty || allAtPos.size == 1
  }) ++ cars.map(c => c.x >= 0 && c.x < width && c.y >= 0 && c.y < height)).forall(_ == true) && find("0").exists(leadCar => cars.filter(c => c.y == (height - 1) / 2 && c.width > c.height).forall(c => c.x <= leadCar.x))

  override def equals(obj: scala.Any): Boolean = obj match {
    case other:Board => this.toString == other.toString
    case _ => false
  }

  override def hashCode(): Int = toString.hashCode
}

object Board {

  /*
  --------
  |   333|
  |    1 |
  | 00 1 =
  |    1 |
  |4 22  |
  |4     |
  --------
   */

  private val rnd = new Random()
  private var maxFringe:Int = 0
  private var searchedNodes:Int = 0

  protected def find(x:Int, y:Int, cars:List[Car]):Option[Car] = cars.find(c => x >= c.x && x < c.x + c.width && y >= c.y && y < c.y + c.height)
  protected def find(pos:(Int, Int), cars:List[Car]):Option[Car] = find(pos._1, pos._2, cars)
  protected def findAll(x:Int, y:Int, cars:List[Car]):List[Car] = cars.filter(c => x >= c.x && x < c.x + c.width && y >= c.y && y < c.y + c.height)

  private def validMoves(car:Car, board:Board):List[(Int, Int)] = {
    if (car.width > car.height){ // left and right
      val right = (1 to (board.width - (car.x + car.width))).map(x => (car.x + x, car.y)).takeWhile(pos => find(pos._1 + (car.width - 1), pos._2, board.cars).forall(_ == car))
      val left = (1 to car.x).map(x => (car.x - x, car.y)).takeWhile(pos => find(pos, board.cars).forall(_ == car))
      (right.lastOption :: left.lastOption :: Nil).flatten
    } else { // up and down
      val down = (1 to (board.height - (car.y + car.height))).map(y => (car.x, car.y + y)).takeWhile(pos => find(pos._1, pos._2 + (car.height - 1), board.cars).forall(_ == car))
      val up = (1 to car.y).map(y => (car.x, car.y - y)).takeWhile(pos => find(pos, board.cars).forall(_ == car))
      (down.lastOption :: up.lastOption :: Nil).flatten
    }
  }

  private def findValidCarPosition(car:Car, others:List[Car], boardWidth:Int, boardHeight:Int):List[(Int, Int)] = {
    (for {
      y <- 0 until (boardHeight - car.height + 1)
      x <- 0 until (boardWidth - car.width + 1)
    } yield {
      if (y == (boardHeight - 1) / 2 && car.width > car.height) None
      else if ((for {
        w <- 0 until car.width
        h <- 0 until car.height
      } yield {
        find(x + w, y + h, others)
      }).flatten.isEmpty) Option((x, y))
      else None
    }).toList.flatten
  }

  def randomBoard():Board = randomBoard(2 + rnd.nextInt(18), 6, 6)

  def randomBoard(maxCars:Int):Board = randomBoard(maxCars, 6, 6)

  def randomBoard(width:Int, height:Int):Board = randomBoard(2 + rnd.nextInt(18), width, height)

  def randomBoard(maxCars:Int, width:Int, height:Int):Board = {
    val cars = Car.randomCars(maxCars)
    var placed = cars.head.at(rnd.nextInt(width - 3), (height - 1) / 2) :: Nil
    cars.tail.foreach{c =>
      findValidCarPosition(c, placed, width, height) match {
        case Nil => () // drop the car if it can't fit
        case positions => placed = placed ++ List(c.at(positions(rnd.nextInt(positions.size))))
      }
    }
    Board(placed, width, height)
  }

  def randomBoard(difficulty: BoardDifficulty, maxAttempts:Int = 50):Option[BoardMeta] = {
    var meta:BoardMeta = null
    var attempts = 0
    while (meta == null || meta.steps < difficulty.minSteps || meta.steps > difficulty.maxSteps){
      if (attempts > maxAttempts) return None
      val board = if (difficulty == BoardDifficulty.INSANE) randomBoard(7, 7) else randomBoard()
      println(s"Found: \n$board")
      meta = solveBoard(board)
      println(s"Solved: \n${meta.metadata}\n")
      attempts += 1
    }
    Option(meta)
  }

  def solveBoard(board:Board):BoardMeta = solveBoard_AStar(board)

  private def solveBoard_AStar(board:Board):BoardMeta = {
    maxFringe = 0
    searchedNodes = 0
    val startTime = System.currentTimeMillis()
    val queue = new mutable.PriorityQueue[SolutionTree]()
    queue.enqueue(new SolutionTree(board))
    val solution = findSolution_AStar(queue)
    val solutionTime = System.currentTimeMillis() - startTime
    val currentFringe = queue.size
    if (solution != null) BoardMeta(board, solution, solutionTime, maxFringe, currentFringe, searchedNodes)
    else BoardMeta.invalid(board, solutionTime, maxFringe, currentFringe, searchedNodes)
  }

  private def findSolution_AStar(fringe:mutable.PriorityQueue[SolutionTree]):SolutionTree = {
    if (fringe.isEmpty) return null
    searchedNodes += 1
    if (fringe.size > maxFringe) maxFringe = fringe.size
    val possible = fringe.dequeue()
    if (!possible.board.isValid) throw new RuntimeException(s"board is not valid: \n${possible.board}")
    if (possible.isSolution) return possible
    possible.board.cars.foreach(car => possible.board.validMoves(car).foreach { pos =>
      val next = new SolutionTree(possible.board.moveCar(car.id, pos))
      if (possible.addChild(next)) fringe.enqueue(next)
    })
    findSolution_AStar(fringe)
  }
}
