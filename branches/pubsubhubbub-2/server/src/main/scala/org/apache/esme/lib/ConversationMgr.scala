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
           Loc.Snippet("displayConversation", displayConversation))) ::
  Nil

  def displayConversation(in: NodeSeq): NodeSeq = {

    val jsId = "timeline_messages"                  

    val cid = S.param("cid").map(toLong).openOr(-1L)

    val msgs = Message.findAndPrime(By(Message.conversation, cid),
                                    OrderBy(Message.id, Descending))

    Script(
      OnLoad(JsCrVar(jsId, JsArray(
          msgs.map(m => JsObj(("message", m.asJs)) ) :_*)) &
      JsFunc(" displayMessages", JsVar(jsId), jsId).cmd)
    )

  }

}
