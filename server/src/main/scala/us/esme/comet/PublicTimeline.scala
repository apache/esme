
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
import net.liftweb.mapper._
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

class PublicTimeline extends CometActor with MsgFormat {

  def defaultPrefix = "timeline"

  private var messages: List[Long] = Nil
  private var lastRender = millis
  private var scheduled = false

  override def localSetup() {
    super.localSetup()
    Distributor ! Distributor.PublicTimelineListeners(this) 
    messages = Message.findAll(OrderBy(Message.id, Descending), 
                               MaxRows(40)).map(_.id.is)
  }
  
  override def localShutdown() {
    super.localShutdown()
    Distributor ! Distributor.PublicTimelineUnlisteners(this)
  }
  
  def render = {
    lastRender = millis
    scheduled = false
    val msgMap = Message.findMessages(messages)
    val toDisplay = messages.flatMap(msgMap.get)
  
    <xml:group>
      {
        toDisplay.map(m => formatMsg(m, true, true))
      }
    </xml:group>
  }
  
  override implicit def xmlToXmlOrJsCmd(in: NodeSeq): RenderOut = new RenderOut(Full(in), fixedRender, Empty, Empty, false)

  override def lowPriority = {
    case ForceRender =>
      reRender(false)

    case Distributor.NewMessage(msg) =>
      messages = (msg.id.is :: messages).take(40)

      if ((millis - lastRender) < 30000L) {
        if (!scheduled) {
          scheduled = true    
          ActorPing.schedule(this, ForceRender, 30000L)
        }
      }
      else reRender(false)
  }
}

case object ForceRender
