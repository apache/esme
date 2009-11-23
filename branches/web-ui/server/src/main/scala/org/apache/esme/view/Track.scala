package org.apache.esme.view

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
/*
import net.liftweb._
import http._
import js._
import JE._
import JsCmds._
import SHtml._
import util._
import Helpers._
import sitemap._
import mapper._
import Loc._

import org.apache.esme._
import model._

import scala.xml.{NodeSeq}

object Track {
  def loggedIn_? = User.loggedIn_?
  
  val ifIsLoggedIn = If(loggedIn_? _, "You must be logged in")

  val menuItems =
  Menu(Loc("trackMgt", List("track", "index"), "Item Tracking", ifIsLoggedIn)) ::
  Nil
}

class Track extends LiftView {
  def dispatch = Map("index" -> index _)
  //{
  //case "index" => index
//}

  def index() =
  for (user <- User.currentUser) yield {
    val spanName = "TokenSpan"
    val theInput = "theInput"
    def redisplayTracking: JsCmd = SetHtml(spanName, displayTracking)

    def addTrack(what: String): JsCmd = {
      what.trim match {
        case x if x.length < 3 => S.error("Too short")
        case x => Tracking.create.regex(x).user(user).saveMe
          S.notice("Now tracking "+x)
      }

      redisplayTracking & SetValById(theInput, "")
    }

    def displayTracking: NodeSeq =
    Tracking.findAll(By(Tracking.user, user), By(Tracking.removed, false),
    OrderBy(Tracking.id, Ascending)) match {
      case Nil => Nil
      case xs =>
        <ul>
          {
            xs.map(at => <li>{at.pattern} Enabled: {ajaxCheckbox(!at.disabled, e => {at.disabled(!e).save; Noop})}
                {SHtml.ajaxButton("Remove", () => {at.removed(true).save ; redisplayTracking})}</li>)
          }
        </ul>
    }

    <lift:surround with="default" at="content">

      Manage your tracking items: <br/>
      <span id={spanName}>
      {
        displayTracking
      }
      </span>

      {
        ajaxForm(
          <xml:group>
            Track something new.<br />
            What? {text("", addTrack) % ("id" -> theInput)}
            <input type="submit" value="Add" />
          </xml:group>
        )
      }

    </lift:surround>
  }
}*/
