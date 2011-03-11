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

package org.apache.esme.snippet          

import org.apache.esme.liftwebext.SHtml._

import org.apache.esme._
import model._    
import lib.MessageUtils
import org.apache.esme.actor._   
import org.apache.esme.comet._

import net.liftweb._
import http._     
import js._
import js.jquery._
import http.jquery._
import JqJsCmds._
import JsCmds._ 
import SHtml._
import JE._    
import util._
import common._
import Helpers._

import mapper._

import scala.xml.{NodeSeq, Text}        
import java.util.Date

object StreamDisplay {    

  val AnyResender = User.id.defaultValue
  val PublicPool = AccessPool.id.defaultValue
  
  val resenderInput = "resender_input"
  val poolInput = "pool_input"
  val filterResentInput = "filter_resent_input"
  val filterPoolsInput = "filter_pools_input" 

  object following extends RequestVar[Seq[(String, String)]] (Nil)
  object pools extends RequestVar[Seq[(String, String)]] (Nil)

  def display(in: NodeSeq): NodeSeq = {
  
    following.set((AnyResender.toString, S.?("base_streams_resend_any")) ::
    (User.currentUser match {
      case Full(u) => u.following.map(u => (u.id.is.toString, u.nickname.is) )
      case _ => Nil
    }))

    pools.set((PublicPool.toString, S.?("base_streams_pool_default")) ::
    (User.currentUser match {
      case Full(u)=> Privilege.findViewablePools(u.id).map(
        p => (p.toString, AccessPool.find(p).get.getName)).toList
      case _ => Nil
    }))

    val name = randomString(20)
                                  
    def cometTimeline:NodeSeq = <lift:comet type="StreamTimeline" name={ name } />    
    
    lazy val timelineActor = StreamTimeline.find(name)
    
    bind("stream", in,                                           
         "cometTimeline" -> cometTimeline,
         
         "resent" -> ajaxSortedSelect(following.is, true, Empty,
           u => {
             for {
               ta <- timelineActor
             } { 
               ta ! SetResender(User.find(u.toLong).openOr(AnyResender))
             }                       
             Run("")
           },
           "id" -> resenderInput),
         
         "pools" -> ajaxSortedSelect(pools.is, true, Empty,
           p => {  
             for {
               ta <- timelineActor
             } {
               ta ! SetPool(AccessPool.find(p).openOr(PublicPool))
             }
             
             Run("")
           },
           "id" -> poolInput),
         
         "filterResent" -> ajaxCheckbox(false,
           r_? => {  
             for(ta <- timelineActor) {
               ta ! FilterResender(r_?)
             }
             Run("")
           },
           "id" -> filterResentInput),
         
         "filterPools" -> ajaxCheckbox(false,
           p_? => {
             for(ta <- timelineActor) {
               ta ! FilterPool(p_?)
             }
             Run("")
           },
           "id" -> filterPoolsInput)
    )
  }       
}
