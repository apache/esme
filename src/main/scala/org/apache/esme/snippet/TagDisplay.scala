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


     bind("user", in,
         "nicename" -> user.niceName,
         "lastName" -> user.lastName,
         "firstName" -> user.firstName,
          "image" -> user.image_url,
         "followButton" -> followOrNot,
         "timeline" -> bindTag(Mailbox.mostRecentMessagesFor(user.id, 50).map(_._1)) _,
         "messages" -> bindTag(Message.findAll(By(Message.author, user), OrderBy(Message.id, Descending), MaxRows(50))) _,
         AttrBindParam("userId", Text(user.id.toString),"userId")
    )

  
  }

  private def bindTag(tagList: List[Message])(in: NodeSeq): NodeSeq =
    tagList.flatMap{m =>
      val nickname = m.author.obj.map(_.nickname.is) openOr ""
      bind("item", in,
           "author" -> <a href={"/user/"+urlEncode(nickname)}>{nickname}</a>,
           "body" -> m.digestedXHTML,
           "date" -> Text(new Date(m.when.is).toString))
    }

  def display(in: NodeSeq): NodeSeq = {  
  
    val name = (S.param("tag") openOr "N/A").trim 
    
    val tag = Tag.findAll(By(Tag.name, name))
    val user = User.currentUser

    val tagList: List[Message] =
    for {
      t <- tag
      item <- t.findMessages()
    } yield item   
    
    def followOrUnfollow: NodeSeq = {          
      val ret: Box[NodeSeq] = for { 
        u <- user
        t = tag.first
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

    bind("tag", in, "name" -> name,
         "each" -> bindTag(tagList) _,
         "followButton" -> followOrUnfollow )

  }  
}
