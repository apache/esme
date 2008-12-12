/*
 * TagUtils.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package us.esme.lib

object TagUtils {

  // Normalize the frequencies from arbitrary Integers to the range (0.0 - 1.0)
  def normalize(llsi: List[(String,Int)]):List[(String,Float)] = {
    val maxVal: Float = llsi.foldLeft(0)(_ max _._2).toFloat
    
    llsi.map{case (name, value) => (name, value.toFloat / maxVal)}
  }
  
  // Compounds a bunch of (String, Int) elements so that [(String1, Int1), (String1, Int2)] becomes [(String1, Int1+Int2)]
  def compound(llsi: List[(String,Int)]): List[(String,Int)] =
    llsi.foldLeft[Map[String, Int]](Map.empty) {
      case (map, (str, cnt)) => map + (str -> (map.getOrElse(str, 0) + cnt))
    }.toList

  
  def everyEven(x:List[(String, Int)]):List[(String, Int)] = everyOther(x)
  def everyOdd(x:List[(String, Int)]):List[(String, Int)] = everyOther(dummy::x)
  
  private val dummy = ("", 0)
  private def everyOther(x:List[(String, Int)]):List[(String, Int)] = {
    x match {
      case Nil => Nil
      case _ :: Nil => Nil
      case _ :: elem :: sublist => elem :: everyOther(sublist)
    }
  }

}
