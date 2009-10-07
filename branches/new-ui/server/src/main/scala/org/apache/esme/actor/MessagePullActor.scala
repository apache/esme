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
import scala.actors.Actor._
import net.liftweb.http.ActorWatcher
import org.apache.esme.actor.Distributor.{UserCreatedMessage=>Msg}

class MessagePullActor(val messageProcessor: Actor, private var lastMessage: Option[Msg], val messageSource: UniqueMessageSource) extends Actor {

  import MessagePullActor._

  def act {
    loop {
      react {
        case StartUp => {
          link(ActorWatcher)
        }
        case ByeBye => {
          unlink(ActorWatcher)
          self.exit()
        }
        case (msgs: List[Msg]) => {
          val lastMessages = messageSource.getLastSortedMessages(msgs, lastMessage)
          for (message <- lastMessages) {
            messageProcessor ! message
            lastMessage = Some(message)
          }
        }
        case FetchMessages => actor {
          // "this" used to reference invoking actor
          this ! messageSource()
        }
      }
    }
  }
  
}

object MessagePullActor extends Actor {
  
  private var messagePullActors: Map[Any, Actor] = Map()
  
  def act = loop {
    react {
      case StartPullActor(obj, lastMessage, messageSource) => {
        if (!messagePullActors.contains(obj)) {
          val pullActor = new MessagePullActor(Distributor, lastMessage, messageSource)
          messagePullActors += (obj -> pullActor)
          pullActor.start
          pullActor ! StartUp
        }
      }
      case StopPullActor(obj) => {
        if (messagePullActors.contains(obj)) {
          messagePullActors(obj) ! ByeBye
          messagePullActors -= obj
        }
      }
      case Fetch(obj) => {
        if (messagePullActors.contains(obj)) {
          messagePullActors(obj) ! FetchMessages
        }
      }
    }
  }

  start

  // do nothing
  def touch {
  }
  
  private case object StartUp
  private case object ByeBye
  private case object FetchMessages
  case class StartPullActor(any: Any, lastMessage: Option[Msg], messageSource: UniqueMessageSource)
  case class StopPullActor(any: Any)
  case class Fetch(any: Any)
}

trait UniqueMessageSource extends (() => List[Msg]) {
  def messageSorter(a: Msg, b: Msg) = a.when < b.when

  def getLastSortedMessages(msgs: List[Msg], lastMessage: Option[Msg]): List[Msg] = {
    lastMessage match {
      case Some(message: Msg) => msgs.filter{messageSorter(message, _)}
      case None => msgs
    }
  }.sort(messageSorter)
}
