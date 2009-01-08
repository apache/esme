
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

class TagCloud extends CometActor /* with MsgFormat*/ {

  private var messages: List[Long] = Nil
  
  override def localSetup() {
    super.localSetup()
    for (user <- User.currentUser) {
      Distributor ! Distributor.Listen(user.id, this)
      Distributor !? (2000, Distributor.LatestMessages(user.id, 40)) match {
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

  private def lookupMessages(): List[Message] = {
    val mm = Message.findMessages(messages)
    messages.flatMap(mm.get)
  }

  def render = {
    val messages = lookupMessages()
    //Sort the tags & words to put the most prominent in the middle
    <div id="tagcloud">
      <p id="tag-para">Tags</p>
      <p>
        {
          for ((name, weight) <- Tag.centreWeightedTopNTagFreqs(messages, 20))
          yield <xml:group><a href={"/tag/" + name}
              style={"font-size: "+(0.5F + weight)+"em; text-decoration: none;"}>{
                name}</a> </xml:group>
        }
      </p>
      <p/>
      <p id="word-para">Words</p>
      <p>
        {
          for ((name, weight) <- Message.centreWeightedTopNWordFreqs(messages, 20))
          yield <xml:group><a href={"/search/" + name}
              style={"font-size: "+(0.5F + weight)+"em; text-decoration: none;"}>{
                name}</a> </xml:group>
        }
      </p>
    </div>
  }


  override def lowPriority = {
    case UserActor.MessageReceived(msg, _) =>
      messages = (msg.id.is :: messages).take(40)
      reRender(false)
  }
}
