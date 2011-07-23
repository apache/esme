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

import net.liftweb.common._ 
import net.liftweb.mapper._  
import net.liftweb.http._      
import net.liftweb.util.Helpers.TimeSpan  

import org.apache.esme._
import actor.{Distributor,UserActor}
import model._   

class UserMessagesTimeline extends Timeline {   

  val jsId = "user_messages_timeline_messages"   
  
  override def lifespan = Full(TimeSpan(300000))    
  
  val user: User = S.param("uid").flatMap(User.findFromWeb) openOr {
    S.error(S.?("base_ui_no_user_found"))
    S.redirectTo(S.referer openOr "/")
  }

  override def localSetup() {
    super.localSetup()                
    Distributor ! Distributor.ListenObject(user, this) 
    messages = Message.findAll(
      By(Message.author, user), 
      OrderBy(Message.id, Descending), 
      MaxRows(40)).map(m => (m.id.is,NoReason,true))          
  }  
  
  override def localShutdown() {
    super.localShutdown()
    for (user <- User.currentUser) {
      Distributor ! Distributor.Unlisten(user.id, this)
    }
  }
  
  override def lowPriority = {
    case UserActor.MessageReceived(msg, r) =>   
      if(msg.author == user) {
        messages = ( (msg.id.is,r,true) :: messages).take(40)   
        prependMessage(msg,r,true)
      }        
      
    case UserActor.Resend(msgId) =>      
      val resendReason = ResendReason(user.id.is)
      messages = ( (msgId,resendReason,true) :: messages).take(40)       
      prependMessage(Message.findMessages(Seq(msgId)).head._2, resendReason, true)
  }   
}