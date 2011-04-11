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

import org.apache.esme._
import actor.{Distributor,UserActor}
import model._                
import net.liftweb.http.js.jquery.JqJsCmds.{PrependHtml}  

class PersonalTimeline extends Timeline {   

  val jsId = "personal_timeline_messages"  

  override def localSetup() {
    super.localSetup()
    for (user <- User.currentUser) {
      Distributor ! Distributor.ListenObject(user, this)
      Distributor !? (2000, Distributor.LatestMessages(user.id, 40)) match {
        case Full(msg: List[(Long,MailboxReason,Boolean)]) => messages = msg
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
  
  override def lowPriority = {
    case UserActor.MessageReceived(msg, r) => {
      messages = ( (msg.id.is,r,false) :: messages).take(40)
      prependMessage(msg,r,false)
    }
    
// TODO: Why do we do this???  
    case UserActor.Resend(msgId) =>
      messages = messages.map {
        case (`msgId`, r, _) => (msgId, r, true)
        case x => x
      }                               
      reRender(false)

// TODO: Why do we do this???      
    case Distributor.UserUpdated(_) => 
      reRender(false)
  }   

}