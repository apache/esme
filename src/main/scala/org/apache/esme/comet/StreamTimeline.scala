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
import net.liftweb.util.Helpers._
import scala.xml._     

import js._
import JsCmds._
import JE._

import org.apache.esme._
import actor.{Distributor}
import model._
import net.liftweb.util.{Schedule, ActorPing}

class StreamTimeline extends PublicTimeline { 
                                                
  var currentResender = StreamTimeline.AnyResender    
  var currentPool = StreamTimeline.PublicPool           
  var filterResender = false
  var filterPool = false    

  override def localSetup() {
    super.localSetup()
    StreamTimeline.register(this)
  }
  
  override def localShutdown() {
    super.localShutdown()
    StreamTimeline.unregister(this)
  }

  override val jsId = "stream_timeline_messages"  
      
  override def lifespan = Full(TimeSpan(300000))    
  
  override def highPriority = {      
    case SetPool(pool) => {  
      pool match {
        case p:AccessPool => currentPool = p.id.is      
        case _ => currentPool = StreamTimeline.PublicPool    
      }
                                             
      if(filterPool) {                     
        messages = getMessages                      
        this ! ForceRender
      }                    
    }
    
    case SetResender(user) => {    
      user match {
        case u:User => currentResender = u.id.is
        case _ => currentResender = StreamTimeline.AnyResender
      }
      
      if(filterResender) {
        messages = getMessages                  
        this ! ForceRender
      }                    
    }
    
    case FilterPool(on) => {
      filterPool = on 
      messages = getMessages                   
      this ! ForceRender
    }
    
    case FilterResender(on) => {
      filterResender = on
      messages = getMessages              
      this ! ForceRender
    }                     
  }    
  
  override def lowPriority = {
    case ForceRender =>                 
      reRender(true)

    case Distributor.NewMessage(msg) =>     
      
      // If we are filtering on pools, only take the message if it is in
      // the pool currently in the filter.
      //
      // Unfortunately we don't currently have a way to filter incoming
      // messages based on resender. So if the resender filter is checked
      // we just drop all messages.
      
      if (
        ((filterPool && msg.pool == currentPool) || !filterPool) &&  
        !filterResender
      )
        messages = ((msg.id.is, NoReason, false) :: messages).take(40)

      if ((millis - lastRender) < 30000L) {
        if (!scheduled) {
          scheduled = true    
          Schedule.schedule(this, ForceRender, 30000L)
        }
      }                                
      else reRender(true)
  }
  
  def getMessages:List[(Long,MailboxReason,Boolean)] = {     
  
    val AnyResender = User.id.defaultValue
    val PublicPool = AccessPool.id.defaultValue
                               
    val user = User.currentUser
  
    val resentQuery = 
      if (filterResender == false) Nil
      else {
        val queryParam = currentResender match {
          case AnyResender => NotBy(Mailbox.resentBy, Empty)
          case id => By(Mailbox.resentBy, id)
        }
        List(In(Message.id,Mailbox.message,By(Mailbox.user, user), queryParam))
      }
    
    val poolsQuery = 
      if (filterPool == false) Nil
      else List(currentPool match {
        case PublicPool => By(Message.pool, Empty)
        case id => By(Message.pool, id)
      })    
    
    val query = poolsQuery :::
                resentQuery :::
                List[QueryParam[Message]](OrderBy(Message.id, Descending), MaxRows(40)) 
             
    Message.findAll(query: _*).map( m => 
      (m.id.is, NoReason, false)
    ) 
  }     
}           

object StreamTimeline {                    
  val AnyResender = User.id.defaultValue
  val PublicPool = AccessPool.id.defaultValue

  private var timelines:Map[Box[String],StreamTimeline] = Map()   
  
  def register(timeline:StreamTimeline) {
    timelines = timelines + (timeline.name -> timeline)    
  }                             
  
  def unregister(timeline:StreamTimeline) {
    timelines = timelines - timeline.name      
  }                                                               
  
  def find(name:String):Box[StreamTimeline] = {
    timelines.get(Full(name))
  }
}          

case class SetPool(pool:Any)
case class SetResender(user:Any) 
case class FilterPool(on:Boolean)
case class FilterResender(on:Boolean)