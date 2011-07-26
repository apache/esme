package org.apache.esme.actor

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
import common._
import actor._
import org.apache.esme.actor.Distributor.{UserCreatedMessage=>Msg}
//import com.twitter.stats.Stats
import com.twitter.ostrich.stats.Stats

object MessagePullActor extends LiftActor {
  
  private var messagePullActors: Map[Long, LiftActor] = Map()
  
  protected def messageHandler = {
    case StartPullActor(obj, lastMessage, messageSource) => 
      if (!messagePullActors.contains(obj)) {
        val pullActor = new MessagePullActor(Distributor, lastMessage, messageSource)
        messagePullActors += (obj -> pullActor)
        pullActor ! StartUp
      }

    case StopPullActor(obj) => 
      messagePullActors.get(obj).foreach { pull =>
        pull ! ByeBye
        messagePullActors -= obj
      }
    
    case Fetch(obj) => 
      messagePullActors.get(obj).foreach (
        _ ! FetchMessages
      )
  }


  // do nothing
  def touch {
  }
  
  private case object StartUp
  private case object ByeBye
  private case object FetchMessages
  case class StartPullActor(id: Long, lastMessage: Option[Msg], messageSource: UniqueMessageSource)
  case class StopPullActor(id: Long)
  case class Fetch(id: Long)


  private class MessagePullActor(
    messageProcessor: LiftActor, 
    private var lastMessage: Option[Msg],
    messageSource: UniqueMessageSource) extends LiftActor {
      
    protected def messageHandler = {
      case StartUp => 
        
      case ByeBye => 
          
      case (msgs: List[Msg]) => 
        val lastMessages = messageSource.getLastSortedMessages(msgs, lastMessage)
        for (message <- lastMessages) {
          messageProcessor ! message
          lastMessage = Some(message)
          Stats incr "messagesPulled"
        }
      
      case FetchMessages => 
        this ! messageSource()
    }
    
  }
}  
  


trait UniqueMessageSource extends (() => List[Msg]) {
  def messageSorter(a: Msg, b: Msg) = a.when < b.when

  def getLastSortedMessages(msgs: List[Msg], lastMessage: Option[Msg]): List[Msg] = {
    lastMessage match {
      case Some(message: Msg) => msgs.filter{messageSorter(message, _)}
      case None => msgs
    }
  }.sortWith(messageSorter)
}
