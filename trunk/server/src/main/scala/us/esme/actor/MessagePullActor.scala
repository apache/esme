package us.esme.actor

/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import scala.actors.Actor
import scala.actors.TIMEOUT
import scala.actors.Actor._
import us.esme.actor.Distributor.{UserCreatedMessage=>Msg}

class MessagePullActor(val messageProcessor: Actor, val refreshSeconds: Int, private var lastMessage: Option[Msg], val messageSource: UniqueMessageSource) extends Actor {

  def act {
    loop {
      reactWithin (refreshSeconds * 1000) {
        case (msgs: List[Msg]) => {
          val lastMessages = messageSource.getLastSortedMessages(msgs, lastMessage)
          for (message <- lastMessages) {
            messageProcessor ! message
            lastMessage = Some(message)
          }
        }
        case TIMEOUT => actor {
          // "this" used to reference invoking actor
          this ! messageSource()
        }
      }
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
  }.sort(messageSorter)
}
