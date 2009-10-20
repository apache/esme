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

import java.util.Date
import java.text.{DateFormat,SimpleDateFormat}

/**
 * Manage the sitemap and related snippets for Authentication Tokens
 */
object AuthMgr {
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("base_error_not_logged_in")))

  val menuItems =
  Menu(Loc("authToken", List("auth_view", "index"), S.?("base_token_menu"), ifIsLoggedIn,
           Loc.Snippet("displayTokens", displayTokens),
           Loc.Snippet("main", mainTokens))) ::
  Nil

  object updateTokens extends RequestVar[() => JsCmd](() => Noop)
  
     //XXX display date, should we have a common dateFormat?
    val dateFormat = new SimpleDateFormat("yyyy/MM/dd")
    def getDateHtml(date: Date) : Text = date match {
     case null => Text(S.?("base_pool_ui_empty_date"))
     case d => Text(dateFormat.format(d))
   }

  def displayTokens(in: NodeSeq): NodeSeq = {
    // get the span name to update
    val spanName = S.attr("the_id") openOr "TokenSpan"
    // get the current user
    val user = User.currentUser

    // bind the dynamic content to the incoming nodeseq
    def doRender(): NodeSeq =
    AuthToken.findAll(By(AuthToken.user, user),
                   OrderBy(AuthToken.id, Ascending)) match {
      case Nil => NodeSeq.Empty
      case xs => bind("disp", in,
                      "item" -> 
                      (lst => xs.flatMap(i => bind("item", lst,
                                                   "description" -> i.description.is,
                                                   "uniqueId" -> i.uniqueId.is,
                                                   "createdDate" -> getDateHtml(i.createdDate),
                                                   "revoke" -> 
                                                   ((bt: NodeSeq) => 
                  ajaxButton(bt, () => {i.delete_! ; updateSpan()}))
              ))))
    }

    def updateSpan(): JsCmd = SetHtml(spanName, doRender())

    updateTokens.set(updateSpan)
    doRender()
  }

  def mainTokens(in: NodeSeq): NodeSeq = {
    val redisplayTokens = updateTokens.is
    val theInput = "token_input"
    val user = User.currentUser
    
    def addAuthToken(desc: String): JsCmd = {
      desc.trim match {
        case x if x.length < 3 => S.error(S.?("base_token_error_name_short"))
        case x => AuthToken.create.description(x).user(user).saveMe
          S.notice(S.?("base_token_msg_new_token"))
      }

      redisplayTokens() & SetValById(theInput, "")
    }

    bind("main", in,
         "token" -> text("", addAuthToken, "id" -> theInput)
    )
    
  }

}
