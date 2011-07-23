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
import actor.{Distributor,UserActor,TagDistributor}
import model._                    

class TagTimeline extends Timeline {  
  
  val jsId = "tag_timeline_messages"
  val tagName = (S.param("tag") openOr "").trim   
  val tag = Tag.findAll(By(Tag.name, tagName))                                       
                                  
  override def localSetup() {
    super.localSetup()          
    
    for(t <- tag) {
      Distributor ! Distributor.ListenObject(t, this)
      messages = t.findMessages().take(40).map(m => (m.id.is, NoReason, true))
    }
  }  
  
  override def localShutdown() {
    super.localShutdown()       
    for(t <- tag) {
      Distributor ! Distributor.UnlistenObject(t, this)     
    }                   
  } 
  
  override def lifespan = Full(TimeSpan(300000))
  
  override def lowPriority = {
    case TagDistributor.MessageReceived(msg, r) =>
      messages = ((msg.id.is,r,true) :: messages).take(40) 
      prependMessage(msg,r,false)
  }   

}