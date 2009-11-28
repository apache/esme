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
import common._
import Helpers._

import model._

import scala.xml._

/**
 * Manage the sitemap and related snippets for Tracking Items
 */
object TrackMgr {
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("base_error_not_logged_in")))

  val menuItems =
  Menu(Loc("trackMgt", List("track_view", "index"), S.?("base_track_menu"), ifIsLoggedIn,
           Loc.Snippet("displayTracking", displayTracking),
           Loc.Snippet("main", mainTracking))) ::
  Nil

  object updateTracking extends RequestVar[() => JsCmd](() => Noop)

  def displayTracking(in: NodeSeq): NodeSeq = {
    // get the span name to update
    val spanName = S.attr("the_id") openOr "TrackSpan"
    // get the current user
    val user = User.currentUser

    // bind the dynamic content to the incoming nodeseq
    def doRender(): NodeSeq =
    Tracking.findAll(By(Tracking.user, user), By(Tracking.removed, false),
    OrderBy(Tracking.id, Ascending)) match {
      case Nil => NodeSeq.Empty
      case xs => bind("disp", in,
                      "item" -> 
                      (lst => xs.flatMap(i => bind("item", lst,
                                                   "pattern" -> i.pattern,
                                                   "enabled" -> ajaxCheckbox(!i.disabled,
                                                                             e => {i.disabled(!e).save; Noop} ),
                                                   "remove" -> 
                                                   ((bt: NodeSeq) => 
                  ajaxButton(bt, () => {i.removed(true).save ; updateSpan()}))
              ))))
    }

    def updateSpan(): JsCmd = SetHtml(spanName, doRender())

    updateTracking.set(updateSpan)
    doRender()
  }

  def mainTracking(in: NodeSeq): NodeSeq = {
    val redisplayTracking = updateTracking.is
    val theInput = "tracking_input"
    val user = User.currentUser
    
    def addTrack(what: String): JsCmd = {
      what.trim match {
        case x if x.length < 3 => S.error(S.?("base_track_error_name_short"))
        case x => Tracking.create.regex(x).user(user).saveMe
          S.notice(S.?("base_track_msg_success", x))
      }

      redisplayTracking() & SetValById(theInput, "")
    }

    bind("main", in,
         "track" -> text("", addTrack, "id" -> theInput)
    )
    
  }

}
