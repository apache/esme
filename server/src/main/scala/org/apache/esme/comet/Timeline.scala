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

package org.apache.esme.comet

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._
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

trait Timeline extends CometActor {
  protected var messages: List[(Long,MailboxReason,Boolean)] = Nil  
  protected var clearMessages: Boolean = false  
  protected val jsId: String  
  
  override def defaultPrefix = Full("timeline")
  
  override def localSetup() {  
    super.localSetup()
  }
  
  override def localShutdown() {
    super.localShutdown()
  }
  
  def render = {
    val msgMap = Message.findMessages(messages map {_._1})
    val toDisplay = for ((id, reason, resent) <- messages;
                         msg <- msgMap.get(id))
                    yield (msg, reason, resent)
               
    // Only clear messages if someone has demanded a complete reset by setting the flag     
    def clearFunc:JsCmd = if(clearMessages) {
      clearMessages = false
      JsFunc("clearMessages", jsId).cmd 
    } else {
      Run("")
    }

    OnLoad(
      clearFunc &
      JsCrVar ("root", Message.root) &
      JsCrVar(jsId, JsArray(
        filter(toDisplay).map{case (msg, reason, resent) =>
                  JsObj(("message",msg.asJs),("reason",reason.asJs), ("resent",resent))
                  } :_*)) &
      JsFunc("displayMessages", JsVar(jsId), jsId, JsVar("root")).cmd
    )
  }                                              
  
  // If we need to filter out some messages on display, override this method.
  def filter(msgs:List[(Message,MailboxReason,Boolean)]):List[(Message,MailboxReason,Boolean)] = {
    msgs 
  }
}
