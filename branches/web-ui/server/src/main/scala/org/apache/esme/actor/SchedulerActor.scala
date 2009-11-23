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

class SchedulerActor(val messageProcessor: Actor, val user: Long, val everySeconds: Int, val reason: MailboxReason) extends Actor {

  import SchedulerActor._
  
  def act {
    sendMessage()
    loop {
      reactWithin (everySeconds * 1000) {
        case StartUp => {
          link(ActorWatcher)
        }
        case ByeBye => {
          unlink(ActorWatcher)
          self.exit()
        }
        case TIMEOUT => {
          sendMessage()
        }
      }
    }
  }
  
  def sendMessage() {
    val now = System.currentTimeMillis
    val dateString = TimeHelpers.toInternetDate(now)
    Message.create.author(user).
                   when(now).
                   source("every").
                   setTextAndTags("Regularly scheduled action: " + dateString, Nil, Empty).
                   foreach{ msg =>
                     // Noisy & can't be rejected
                     // if (msg.save) {
                       messageProcessor ! Distributor.AddMessageToMailbox(user, msg, reason)
                     // }
                   }
  }
  
}

object SchedulerActor extends Actor{
  
  private var regularsPerAction: Map[Long, List[Actor]] = Map()
  
  def act = loop {
    react {
      case StartRegular(action, everySeconds) => {
        val id = action.id.is
        val regularActor = new SchedulerActor(Distributor, action.user, everySeconds, RegularReason(id))
        if (!regularsPerAction.contains(id)) {
          regularsPerAction += (id -> List(regularActor))
        } else {
          regularsPerAction(id) += regularActor
        }
        regularActor.start
        regularActor ! StartUp
      }

      case StopRegular(id) => {
        if (regularsPerAction.contains(id)) {
          for(regularActor <- regularsPerAction(id)) regularActor ! ByeBye
          regularsPerAction -= id
        }
      }
    }
  }

  start

  // do nothing
  def touch {
  }
  
  case class StartRegular(action: Action, everySeconds: Int)
  case class StopRegular(action: Long)
  private case object StartUp
  private case object ByeBye
}
