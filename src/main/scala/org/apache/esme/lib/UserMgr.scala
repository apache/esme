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
import Helpers._

import model._

import scala.xml._

/**
 * Manage the sitemap and related snippets for the display of users
 */
object UserMgr {
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("You must be logged in")))

  val menuItems =
  Menu(Loc("list_users", List("info_view", "users"), "List Users", ifIsLoggedIn,
           Loc.Snippet("displayUsers", displayUsers))) ::
  Nil

  def displayUsers(in: NodeSeq): NodeSeq = {
    val users = User.findAll(OrderBy(User.nickname, Ascending))
    bind("disp", in,
         "item" -> 
         (lst => users.flatMap(u => {
           val msg = lastMessage(u.id) 
           bind("item", lst,
             "nickname" -> u.nickname,
             "firstName" -> u.firstName,
             "lastName" -> u.lastName,
             "imageUrl" -> profileImage(u.imageUrl),
             "lastMsg" -> msg._1,
             "lastMsgWhen" -> msg._2
             )
         }
     )))
  }
  
  private def lastMessage(id: Long): Tuple2[String,String] = {
    Mailbox.mostRecentMessagesFor(id, 1) match {
      case Nil => ("", "")
      case msgs => {
        val msg = msgs(0)._1
        (msg.getText, msg.getWhen.toString)
      }
    }
  }

  private def profileImage(imageUrl: String): NodeSeq = {
    if (imageUrl.length > 0) 
      <img src={imageUrl}/>
    else
      <p/>
  }

}
