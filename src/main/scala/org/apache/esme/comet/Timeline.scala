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

package org.apache.esme.comet

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._
import js._
import JsCmds._
import JE._

import org.apache.esme._
import org.apache.esme.actor._
import model._
import lib._

import java.text._

class Timeline extends CometActor {
  private var messages: List[(Long,MailboxReason)] = Nil
  
  override def localSetup() {
    super.localSetup()
    for (user <- User.currentUser) {
      Distributor ! Distributor.Listen(user.id, this)
      Distributor !? (2000, Distributor.LatestMessages(user.id, 40)) match {
        case Some(msg: List[(Long,MailboxReason)]) => messages = msg
        case x =>
      }
    }
  }
  
  override def localShutdown() {
    super.localShutdown()
    for (user <- User.currentUser) {
      Distributor ! Distributor.Unlisten(user.id, this)
    }
  }
  
  def render = {
    val msgMap = Message.findMessages(messages map {_._1})
    val toDisplay =
      for ((id, reason) <- messages;
           msg <- msgMap.get(id))
      yield (msg, reason)
    val jsId = "timeline_messages";

    OnLoad(JsCrVar(jsId, JsArray(
        toDisplay.map(m => JsObj(("message",m._1.asJs),("reason",m._2.asJs))) :_*)) &
    JsFunc("displayMessages", JsVar(jsId), jsId).cmd)
  }
  
  override def lowPriority = {
    case UserActor.MessageReceived(msg, r) =>
      messages = ( (msg.id.is,r) :: messages).take(40)
      reRender(false)
      
    case Distributor.UserUpdated(_) =>
      reRender(false)
  }
}
