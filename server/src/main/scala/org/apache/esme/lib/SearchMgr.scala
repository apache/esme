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
 * Manage the sitemap and related snippets for Search
 */
object SearchMgr {
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("base_error_not_logged_in")))

  val menuItems =
  Menu(Loc("search", List("info_view", "search"), S.?("base_menu_search"), ifIsLoggedIn,
           Loc.Snippet("displaySearch", displaySearch),
           Loc.Snippet("searchTerm", searchTerm))) ::
  Nil

  def searchTerm(in: NodeSeq): NodeSeq = {
    Text(S.param("term").getOrElse("no words specified"))
  }
  
  def displaySearch(in: NodeSeq): NodeSeq = {

    val jsId = "search_timeline_messages";
    
    {Script(
      (for (term <- S.param("term");
           user <- User.currentUser)
      yield {

        val msgs = Message.search(term, user.following, 50)    
        OnLoad(JsCrVar(jsId, JsArray(
            msgs.map(m => JsObj(("message", m.asJs)) ) :_*)) &
        JsFunc("displayMessages", JsVar(jsId), jsId).cmd)
      }).getOrElse(Noop)
    )}

  }

}
