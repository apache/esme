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

import net.liftweb.actor._
import org.apache.esme.actor.Distributor.AddMessageToMailbox
import org.apache.esme.model._
import net.liftweb.http.ActorWatcher
import net.liftweb.util._
import Helpers._
import net.liftweb.common._
import com.twitter.ostrich.stats.Stats


object SchedulerActor extends LiftActor{
  
  private var regularsPerAction: Map[Long, List[LiftActor]] = Map()
  
  protected def messageHandler = {
    case StartRegular(action, everySeconds) => {
      val id = action.id.is
      val regularActor = new SchedulerActor(Distributor, action.user, everySeconds, RegularReason(id))
      if (!regularsPerAction.contains(id)) {
        regularsPerAction += (id -> List(regularActor))
      } else {
        regularsPerAction.updated(id,  regularActor :: regularsPerAction(id))
      }
    }
    
    case StopRegular(id) => {
      if (regularsPerAction.contains(id)) {
        for(regularActor <- regularsPerAction(id)) regularActor ! ByeBye
        regularsPerAction -= id
      }
    }
  }


  private class SchedulerActor(messageProcessor: LiftActor, 
                               user: Long, 
                               everySeconds: Int, 
                               reason: MailboxReason) extends LiftActor {
    private case object DoItDude
    private var running = true
    
    private def setupPing() {
      if (running)
        Schedule.schedule(this, DoItDude, everySeconds * 1000L)
    }
    
    sendMessage()
    setupPing()
    
    protected def messageHandler: PartialFunction[Any, Unit] = {
      case DoItDude =>
        if (running) {
          sendMessage()
          setupPing()
        }

      case ByeBye =>
        running = false
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
      Stats incr "schedulerMessagesCreated"
      Stats incr "messagesCreated"

    }
    
  }
  
  
  // do nothing
  def touch {
  }
  
  case class StartRegular(action: Action, everySeconds: Int)
  case class StopRegular(action: Long)
  private case object ByeBye
}
