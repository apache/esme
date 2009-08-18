package org.apache.esme.actor

/**
 * Copyright 2008-2009 WorldWide Conferencing, LLC
 * 
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

import scala.actors.Actor
import scala.actors.TIMEOUT
import scala.actors.Actor._
import org.apache.esme.actor.Distributor.AddMessageToMailbox
import org.apache.esme.model._
import net.liftweb.http.ActorWatcher
import net.liftweb.util.{Full,Empty,TimeHelpers}

class PopStatsActor(val period: Long, val refreshInterval: Long) extends Actor {

  import PopStatsActor._

  var queue: List[StatEvent] = List()
  var stats: Map[Long,Int] = Map()
  var refreshActor: Actor = _
  
  def act {
    loop {
      react {
        case StartUp =>
          refreshActor = actor {
            loop {
              self.reactWithin(refreshInterval) {
                case TIMEOUT => this ! Expire
                case ByeBye => self.exit()
              }
            }
          }
        case ByeBye =>
          refreshActor ! ByeBye  
          self.exit()
        case Hit(id) =>
          queue += StatEvent(id, now)
          stats += (id -> (stats.getOrElse(id,0) + 1))
        case Top(n) =>
          val topList = stats.toList.sort{
            case ((_,freq1),(_,freq2)) =>
              freq2 < freq1
          }.take(n)
          reply(topList)
        case Expire => {
          queue = queue.dropWhile{ event =>
            val expired_? = event.when < now
            if (expired_?) stats -= event.id
            expired_?
          }.toList
        }
      }
    }
  }
  
  case class StatEvent(id: Long, when: Long)
}

object PopStatsActor extends Actor {

  // immutable maps of maps doesn't work well
  import scala.collection.mutable.Map
  
  val actors: Map[StatParam,Map[Long,PopStatsActor]] = Map()
  
  def now = System.currentTimeMillis
  def act = loop {
    react {
      case StartStats(what, period, refresh) =>
        if (!actors.contains(what))
          actors(what) = Map()
        val stat = actors(what)
        if(!stat.contains(period)) {
          val statActor = new PopStatsActor(period, refresh)
          stat(period) = statActor
          statActor.start
          statActor ! StartUp
        }
          
      case StopStats(what, period) => // TODO: not used
      case TopStats(what, n, period) =>
        (for (stat <- actors.get(what);
             availableActor <- stat.get(period)) yield availableActor) match {
               case Some(statActor) => statActor forward Top(n)
               case _ => reply(Nil)
             }
      case IncrStats(what, hitItem) =>
        for (stat <- actors.get(what);
             statActor <- stat.values)
          statActor ! Hit(hitItem)
    }
  }
  
  start

  // do nothing
  def touch {
  }

  case object StartUp
  case object ByeBye
  case class Top(n: Int)
  case class Hit(id: Long)
  case object Expire
  
  case class StartStats(what: StatParam, period: Long, refresh: Long)
  case class StopStats(what: StatParam, period: Long)
  case class TopStats(what: StatParam, n: Byte, period: Long)
  case class IncrStats(what: StatParam, hitItem: Long)
}

sealed trait StatParam
case object ResendStat extends StatParam
case object LinkClickedStat extends StatParam
