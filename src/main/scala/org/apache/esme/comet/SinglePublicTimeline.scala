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
import net.liftweb.mapper._
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

class SinglePublicTimeline extends Timeline {       

  val jsId = "single_public_timeline_messages"
                                             
  protected var lastRender = millis
  protected var scheduled = false

  override def localSetup() {
    super.localSetup()
    Distributor ! Distributor.PublicTimelineListeners(this) 
    messages = Message.findAll(By(Message.pool, Empty),
        OrderBy(Message.id, Descending), 
                               MaxRows(1)).map( m => (m.id.is, NoReason, true))
  }
  
  override def localShutdown() {
    super.localShutdown()
    Distributor ! Distributor.PublicTimelineUnlisteners(this)
  }
  
  override def render = {
    lastRender = millis
    scheduled = false  
    
    super.render
  }

  override def lowPriority = {
    case SingleForceRender =>                   
      reRender(true)

    case Distributor.NewMessage(msg) =>
      if (!msg.pool.defined_?)
        messages = ((msg.id.is, NoReason, false) :: messages).take(1)

      if ((millis - lastRender) < 30000L) {
        if (!scheduled) {
          scheduled = true    
          Schedule.schedule(this, SingleForceRender, 30000L)
        }
      }                                  
      else reRender(true)
  } 
  
// TODO Should be factored out into a template  
  override val messageTemplate = 
    <div class="single-updates-box" id="message"> 
    	<div class="avatar">
    		<img id="avatar" width="40px" src=""/>
    	</div>
    	<div class="update2">
    		<a class="author"/>
    		<div class="msgbody"/>
      	<div class="supp_data">
      	  <span class="supp_pool"/>
      	  <span class="supp_date"/>
      	  <span class="supp_millidate" style="display:none"/>
      	  <span class="supp_reason"/>
      	</div>				
    	</div>
    </div>    
}

case object SingleForceRender
