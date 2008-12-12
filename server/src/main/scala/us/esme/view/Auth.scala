package us.esme.view

/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */


import net.liftweb._
import http._
import js._
import JE._
import JsCmds._
import SHtml._
import util._
import Helpers._
import sitemap._
import Loc._

import us.esme._
import model._

import scala.xml.{NodeSeq}

object Auth {
  def loggedIn_? = User.loggedIn_?
  
  val ifIsLoggedIn = If(loggedIn_? _, "You must be logged in")

  val menuItems =
  Menu(Loc("authToken", List("auth", "index"), "Manage Tokens", ifIsLoggedIn)) ::
  Nil
}

class Auth extends LiftView {
  def dispatch = Map("index" -> index _)

  def index() =
  for (user <- User.currentUser) yield {
    val spanName = "TokenSpan"
    val theInput = "theInput"
    def redisplayTokens: JsCmd = SetHtml(spanName, displayTokens)

    def addAuthToken(desc: String): JsCmd = {
      desc.trim match {
        case x if x.length < 3 => S.error("Description too short")
        case x => AuthToken.create.description(x).user(user).saveMe
          S.notice("New token added")
      }

      redisplayTokens & SetValById(theInput, "")
    }

    def displayTokens: NodeSeq =
    user.authTokens match {
      case Nil => Nil
      case xs =>
        <ul>
          {
            xs.map(at => <li>{at.description} token: {at.uniqueId}
                {SHtml.ajaxButton("Revoke",
                                  () => {at.delete_!
                                             redisplayTokens})}</li>)
          }
        </ul>
    }

    <lift:surround with="default" at="content">

      Manage your external application authentication tokens: <br/>
      <span id={spanName}>
      {
        displayTokens
      }
      </span>

      {
        ajaxForm(
          <xml:group>
            Create a new Authentication Token.<br />
            Description: {text("", addAuthToken) % ("id" -> theInput)}
            <input type="submit" value="Add" />
          </xml:group>
        )
      }

    </lift:surround>
  }
}
