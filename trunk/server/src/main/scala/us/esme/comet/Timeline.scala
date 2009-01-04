
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
package us.esme.comet

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._
import js._
import JsCmds._
import JE._

import us.esme._
import actor._
import model._
import lib._

import java.text._

class Timeline extends CometActor {
  private var messages: List[Long] = Nil
  
  override def localSetup() {
    super.localSetup()
    for (user <- User.currentUser) {
      Distributor ! Distributor.Listen(user.id, this)
      Distributor !? (200, Distributor.LatestMessages(user.id, 40)) match {
        case Some(msg: List[Long]) => messages = msg
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
    val msgMap = Message.findMessages(messages)
    val toDisplay = messages.flatMap(msgMap.get)
    val jsId = "timeline_messages";

    OnLoad(JsCrVar(jsId, JsArray(
        toDisplay.map(_.asJs) :_*)) &
    JsFunc("displayMessages", JsVar(jsId), jsId).cmd)
  }
  
  override def lowPriority = {
    case UserActor.MessageReceived(msg, _) =>
      messages = (msg.id.is :: messages).take(40)
      reRender(false)
      
    case Distributor.UserUpdated(_) =>
      reRender(false)
  }
}
