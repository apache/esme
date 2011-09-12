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

package org.apache.esme.lib

import net.liftweb._
import http._
import SHtml._
import js._
import JsCmds._
import JE._

import sitemap._
import Loc._

import mapper._

import util._
import common._
import Helpers._

import org.apache.esme._
import model._

import scala.xml._

/**
 * Manage the sitemap and related snippets for Conversations
 */
object ConversationMgr {
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("base_error_not_logged_in")))

  val menuItems =
  Menu(Loc("conversation", List("info_view", "conversation"), S.?("base_conv_menu"), ifIsLoggedIn,
           Loc.Snippet("conv_info", convInfo _))) :: Nil
  
  def convInfo(in: NodeSeq): NodeSeq = {  

    val cid = S.param("cid").map(toLong).openOr(-1L)
    val messages = Message.findMessages(List(cid))   
    val user = User.currentUser 
    var msgPool:Long = 0 

    def followOrUnfollow: NodeSeq = {          
      val ret: Box[NodeSeq] = for { 
        u <- user
        m = messages.values.toList.head
      } yield {         
        if (!m.followers.contains(u)) {
          ajaxButton("Follow conversation", () => {  
            m.followers += u
            m.save       
            updateFollow
          })
        } else {
          ajaxButton("Unfollow conversation", () => {    
            m.followers -= u
            m.save
            updateFollow
          })
        }  
      }
                         
      ret.open_!                                     
    }      
    
    def cometTimeline = <lift:comet type="ConversationTimeline" name={"conv"+cid} /> 
                           
    def updateFollow: JsCmd = SetHtml("following", followOrUnfollow)

    bind("conv", in,       
         "conversationId" -> cid,
         "messagePool" -> messages.values.toList.head.pool.is,
         "cometTimeline" -> cometTimeline,
         "followButton" -> followOrUnfollow )       
  }                                
}
