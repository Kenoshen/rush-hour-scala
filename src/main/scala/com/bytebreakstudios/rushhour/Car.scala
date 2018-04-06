package com.bytebreakstudios.rushhour

import scala.util.Random

case class Car(id:String, x:Int, y:Int, width:Int, height:Int) {
  def withId(id:String):Car = copy(id = id)
  def at(x:Int, y:Int):Car = copy(x = x, y = y)
  def at(pos:(Int,Int)):Car = copy(x = pos._1, y = pos._2)
}

object Car {
  val leadCarPreset = Car("0", 0, 2, 2, 1)
  val horizontalCarPreset = Car("1", 0, 0, 2, 1)
  val verticalCarPreset = Car("1", 0, 0, 1, 2)
  val horizontalTruckPreset = Car("1", 0, 0, 3, 1)
  val verticalTruckPreset = Car("1", 0, 0, 1, 3)

  private val rnd = new Random()

  private val presets = List(horizontalCarPreset, verticalCarPreset, horizontalTruckPreset, verticalTruckPreset)

  private val alpha = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
  private def intToId(i:Int):Option[String] = {
    if (i < 0) None
    else if (i < 10) Option(s"$i")
    else if (i - 10 < alpha.size) Option(alpha(i - 10))
    else None
  }

  def randomCars(count:Int):List[Car] = leadCarPreset :: (1 until count).flatMap(intToId).map(id => randomPreset().withId(id)).toList

  def randomPreset():Car = presets(rnd.nextInt(presets.size))
}
