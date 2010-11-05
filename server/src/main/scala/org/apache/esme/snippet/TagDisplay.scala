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

import org.apache.esme._
import model._    
import lib.MessageUtils
import org.apache.esme.actor._

import net.liftweb._
import http._
import SHtml._
import js._
import JsCmds._
import JE._
import util._
import common._
import Helpers._

import mapper._

import scala.xml.{NodeSeq, Text}
import java.util.Date

object TagDisplay {

  def display(in: NodeSeq): NodeSeq = {  
  
    val name = (S.param("tag") openOr "N/A").trim 
    
    val tag = Tag.findAll(By(Tag.name, name)).headOption
    val user = User.currentUser

    val messageList: List[Message] = 
    {for(t <- tag) yield { t.findMessages() }}.getOrElse(List())
    
    def followOrUnfollow: NodeSeq = {          
      val ret: Box[NodeSeq] = for { 
        u <- user
        t <- tag
      } yield {                 
        if (!t.followers.contains(u)) {
          ajaxButton("Follow tag", () => {  
            t.followers += u
            t.save       
            updateFollow
          })
        } else {
          ajaxButton("Unfollow tag", () => {    
            t.followers -= u
            t.save
            updateFollow
          })
        }  
      }
                             
      ret.open_!                                      
    }  
    
    def updateFollow: JsCmd = SetHtml("following", followOrUnfollow)
    
    def cometTimeline:NodeSeq = { for(t <- tag) yield {
      <lift:comet type="TagTimeline" name={"tag"+t.id.is} />
    }}.getOrElse(Text(""))

    bind("tag", in,
         "name" -> name,
         "each" -> MessageUtils.bindMessages(messageList) _,   
         "cometTimeline" -> cometTimeline,
         "followButton" -> followOrUnfollow )

  }       
}
