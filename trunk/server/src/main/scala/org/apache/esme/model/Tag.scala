package org.apache.esme.model

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

import net.liftweb._
import http._
import mapper._
import util._
import actor._
import common._
import Helpers._

import scala.collection.mutable.HashMap

import org.apache.esme.lib.TagUtils

object Tag extends Tag with MetaProtoTag[Tag] {
  override def dbTableName = "tag" // define the DB table name

  def cacheSize = 500
  
  val logger: Logger = Logger("org.apache.esme.model")
  
  private var listeners: List[LiftActor] = Nil
  private var cloud: List[(String, Float)] = Nil

  // Compounds a bunch of (String, Int) elements so that [(String1, Int1), (String1, Int2)] becomes [(String1, Int1+Int2)]
  private[model] def compound(llsi: List[(String,Int)]): List[(String,Int)] = {
    llsi.foldLeft[Map[String, Int]](Map.empty){
        case (map, (str, cnt)) =>
          map + (str -> (map.getOrElse(str, 0) + cnt))
      }.toList
  }

  def centreWeightedTopNTagFreqs(messages: List[Message], n:Int):List[(String, Float)] = {
    val weights = compound(messages.flatMap(_.tagFrequencies))

    // Start with the top 20 tags, sorted by frequency
    val sortedWeights = weights.sortWith(_._2 > _._2).take(n)

    // And create a normalized cente-weighted list, e.g. smallest, small, Larger, BIG, *HUGE*, BIG, Larger, small, smallest
    TagUtils.normalize(TagUtils.everyEven(sortedWeights).reverse ::: TagUtils.everyOdd(sortedWeights))
  }
  
}

class Tag extends ProtoTag[Tag] with ManyToMany {
  def getSingleton = Tag // what's the "meta" server    
  
  def findMessages(): List[Message] =
  Message.findAndPrime(In.fk(MessageTag.message, By(MessageTag.tag, this)),
		       OrderBy(Message.id, Descending))

  override def toXml = <tag id={id.is.toString} name={name.is}>{"#" + name.is}</tag>    
                                                                                           
  object followers extends MappedManyToMany(UserTagFollow, UserTagFollow.tag, UserTagFollow.user, User)                                                                                         
}               
