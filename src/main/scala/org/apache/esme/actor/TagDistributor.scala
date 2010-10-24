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

package org.apache.esme.actor  

import net.liftweb._  
import actor._        

import org.apache.esme._
import model.{Tag, User, Message, TagFollowReason, NoReason, MailboxReason}       

import scala.collection.mutable.HashMap   

object TagDistributor extends LiftActor {
                             
  protected def messageHandler = {
    case Distributor.NewMessage(msg) => {  
      msg.tagIds.map( i => {    
        Tag.find(i).map( t => {        
          t.followers.refresh.map( u => {  
            Distributor ! Distributor.AddMessageToMailbox(u.id, msg, TagFollowReason(t.name));  
          })
          listeners.getOrElse(t, List()).map(
            _ ! MessageReceived(msg, NoReason)
          )
        })
      })                                                            
    }
    
    case Listen(tag, who) =>
      listeners.update(tag, who :: listeners.getOrElse(tag, List()))
      
    case Unlisten(tag, who) =>
      listeners.update(tag, listeners.getOrElse(tag, List()).filter( _ ne who))
  } 
  
  def touch {
  
  }     
  
  private var listeners: HashMap[Tag,List[LiftActor]] = new HashMap
  
  case class Listen(tag:Tag,who:LiftActor)
  case class Unlisten(tag:Tag,who:LiftActor)      
  case class MessageReceived(msg: Message, reason: MailboxReason)              

}