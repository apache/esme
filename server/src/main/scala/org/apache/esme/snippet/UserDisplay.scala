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

object UserDisplay {
  def userInfo(in: NodeSeq): NodeSeq = {
    val user: User = S.param("uid").flatMap(User.findFromWeb) openOr {
      S.error(S.?("base_ui_no_user_found"))
      S.redirectTo(S.referer openOr "/")
    }
   
    def updateFollow: JsCmd = SetHtml("following", followOrNot)

    def followOrNot: NodeSeq = {
      User.currentUser match {
        case Full(u) if u != user =>
          if (u.following_?(user))
          ajaxButton(S.?("base_ui_unfollow"), () => {u.unfollow(user); updateFollow})
          else ajaxButton(S.?("base_ui_follow"), () => {u.follow(user); updateFollow})

        case _ => <xml:group> <div class="thatsyou">{S.?("base_user_thats_you")}</div></xml:group> 
      }
    }    
    
    def cometTimeline:NodeSeq = {                                        
      //println(<lift:comet type="UserMessagesTimeline" name={"user"+user.id.is} />)
      <lift:comet type="UserMessagesTimeline" name={"user"+user.id.is} />
    }
                                                      
     bind("user", in,
         "nicename" -> user.niceName,
         "lastName" -> user.lastName,
         "firstName" -> user.firstName,
         "image" -> user.image_link,   
         "cometTimeline" -> cometTimeline,
         "followButton" -> followOrNot,
          "messages" -> MessageUtils.bindMessages(Message.findAll(
            By(Message.author, user), 
            OrderBy(Message.id, Descending), 
            MaxRows(50))) _,
         AttrBindParam("userId", Text(user.id.toString),"userId")
    )
 
  }
}