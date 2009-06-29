/**
 * Copyright 2008-2009 WorldWide Conferencing, LLC
 * 
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
import js._
import JsCmds._
import JE._
import util._
import Helpers._

import scala.xml.{NodeSeq, Text, Node}

object JsonPoster extends SessionVar(S.buildJsonFunc{
    case JsonCmd("post", _, map: Map[String, Any], _) =>
      println("Posting "+map)
      for (msgObj <- map.get("msg");
           msg <- Box.asA[String](msgObj).map(_.trim) if msg.length > 0;
           tagObj <- map.get("tags");
           tags <- Box.asA[String](tagObj);
           user <- User.currentUser) {

        val replyTo: Box[Long] = map.get("reply-to").map(toLong)

        val pool = map.get("access_pool").map(toLong) match {
          case Some(x) if x > 0L => Some(x)
          case _ => None
        }
        
        Distributor ! 
        Distributor.UserCreatedMessage(user.id, msg, 
                                       Tag.split(tags),
                                       millis, 
                                       Empty,
                                       "web",
                                       replyTo,
                                       pool)
      }
      Noop

    case _ => Noop
  }
)

class UserSnip extends DispatchSnippet {
  def dispatch: DispatchIt = 
  Map("name" -> userName _,
      "postScript" -> postScript _,
      "followers" -> followers _,
      "following" -> following _,
      "loginForm" -> loginForm _,
      "loggedIn" -> loggedInFilter _,
      "accessPools" -> accessPools _)

  def loggedInFilter(in: NodeSeq): NodeSeq = {
    val lookFor = if (User.loggedIn_?) "in" else "out"
    
    (<foo>{in}</foo> \ lookFor).toList.
    filter(_.prefix == "logged").
    map(_.child).firstOption.getOrElse(NodeSeq.Empty)
  }

  def userFmt(u: User): Node = 
  <li><a href={"/user/"+urlEncode(u.nickname.is)}>{u.niceName}</a> {u.firstName} {u.lastName}</li>

  def calcUser: Box[User] =
  S.attr("userId").flatMap(s => User.find(toLong(s))) or User.currentUser

  def followers(in: NodeSeq): NodeSeq = 
  <ul>
    {
      calcUser.toList.flatMap(_.followers.map(userFmt))
    }
  </ul>

  def following(in: NodeSeq): NodeSeq =
  <ul>
    {
      calcUser.toList.flatMap(_.following.map(userFmt))
    }
  </ul>

  def loginForm(in: NodeSeq): NodeSeq =
  if (User.loggedIn_?) NodeSeq.Empty
  else User.loginForm
  

  def userName(in: NodeSeq) = {
    if (User.currentUser.map(_.needsChange_?) openOr false)
    S.redirectTo("/user_mgt/edit")
    
    Text(User.currentUser.map(_.wholeName) openOr "")
  }

  def accessPools(in: NodeSeq): NodeSeq = {
    for(user <- User.currentUser.toSeq;
        p    <- Privilege.findWritablePools(user.id))
        // slow?
      yield <option value={p}>
              {AccessPool.find(p).get.getName}
            </option>
  }
  
  def postScript(in: NodeSeq): NodeSeq =
  <xml:group>
    {Script(JsonPoster.is._2)}
    {Script(Function("post_msg", List(),
                     JsonPoster.is._1("post",
                                      JsObj("msg" -> ValById("textdude"),
                                            "tags" -> ValById("tagdude"),
                                            "access_pool" -> ValById("access_pool"),
                                            "reply-to" -> JsVar("currentConvNumber"))) &
                     SetValById("textdude", "") &
                     SetValById("tagdude", "") &
                     SetValById("access_pool", "0") &
                     JsRaw("clearReplyTo();")
        ))
    }
  </xml:group>
}
