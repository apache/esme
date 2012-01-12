/**
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.esme.lib

import scalaz._
import Scalaz._

object TagUtils {

  // Normalize the frequencies from arbitrary Integers to the range (0.0 - 1.0)
  def normalize(llsi: List[(String,Int)]):List[(String,Float)] = {
    val maxVal: Float = llsi.foldLeft(0)(_ max _._2).toFloat
    
    llsi.map{case (name, value) => (name, value.toFloat / maxVal)}
  }
  
  // Compounds a bunch of (String, Int) elements so that [(String1, Int1), (String1, Int2)] becomes [(String1, Int1+Int2)]
  // foldMap with List as Foldable and append operation of Map Monoid
  def compound(llsi: List[(String,Int)]): List[(String,Int)] = llsi.foldMap(Map(_)).toList

  
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
