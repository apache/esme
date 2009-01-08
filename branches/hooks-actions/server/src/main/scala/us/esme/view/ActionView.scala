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
import mapper._
import Loc._

import us.esme._
import model._

import scala.xml.{NodeSeq}

object ActionView {
  def loggedIn_? = User.loggedIn_?
  
  val ifIsLoggedIn = If(loggedIn_? _, "You must be logged in")

  val menuItems =
  Menu(Loc("actionMgt", List("action_view", "index"), "Action Management", ifIsLoggedIn)) ::
  Nil
}

class ActionView extends LiftView {
  def dispatch = Map("index" -> index _)


  def index() =
  for (user <- User.currentUser) yield {
    val spanName = "TokenSpan"
    val theName = "theName"
    val theAction = "theAction"
    val theTest = "theTest"

    var testText = ""
    var nameText = ""

    def redisplayActions: JsCmd = SetHtml(spanName, displayActions)
    def saveIt(actionText: String): JsCmd = {

      val toSave = Action.create.name(nameText).user(user)
      DB.use(toSave.connectionIdentifier) {
        ignore =>
        val act: Box[List[FieldError]] =
        for (a1 <- toSave.setAction(actionText);
             a2 <- a1.setTest(testText))
        yield a2.validate

        act match {
          case Full(Nil) =>
            toSave.save
            S.notice("Action added")
            redisplayActions &
            SetValById(theName, "") & SetValById(theAction, "") &
            SetValById(theTest, "")

          case Full(xs) => S.error(xs); Noop
          case Failure(msg, _, _) => S.error(msg) ; Noop
          case _ => Noop
        }
      }
    }

    def displayActions: NodeSeq =
    Action.findAll(By(Action.user, user), By(Action.removed, false),
                   OrderBy(Action.id, Ascending)) match {
      case Nil => Nil
      case xs =>
        <table>
          <tr>
            <td>Name</td>
            <td>Enabled</td>
            <td>Test</td>
            <td>Action</td>
            <td>Remove</td>
          </tr>
          {
            xs.map(at =>
              <tr>
                <td>{at.name}</td>
                <td>Enabled: {ajaxCheckbox(!at.disabled, e => {at.disabled(!e).save; Noop})}</td>
                <td><pre>{at.testText}</pre></td>
                <td><pre>{at.actionText}</pre></td>
                <td>
                  {SHtml.ajaxButton("Remove", () => {at.removed(true).save ; redisplayActions})}
                </td>
              </tr>)
          }
        </table>
    }

    <lift:surround with="default" at="content">

      Manage your Actions items: <br/>
      <span id={spanName}>
        {
          displayActions
        }
      </span>

      {
        ajaxForm(
          <xml:group>
            <table>
              <tr><td colspan="3">New Action</td></tr>
              <tr>
                <td>Name</td>
                <td>{text("", nameText = _) % ("id" -> theName)}</td>
              </tr>
              <tr>
                <td>Test</td>
                <td>{textarea("", testText = _) % ("id" -> theTest)}</td>
                <td>
                  @foo -- sender is @foo<br />
                  day = (0,1) -- sent on Sunday or Monday<br/>
                  #moo -- contains the #moo tag<br/>
                  50% -- success 50% of the time<br/>
                  @foo &amp; 50% -- half the time, something sent by @foo
                </td>
              </tr>

              <tr>
                <td>Action</td>
                <td>{textarea("", saveIt) % ("id" -> theAction)}</td>
                <td>
                  filter -- not put in your timeline<br />
                  resend -- sends the message to all your followers<br />
                  mailto:foo@bar.com -- sends the message to foo@bar.com<br/>
                  http://foo.com/message/in -- makes an HTTP post with the message
                  </td>
              </tr>
              <input type="submit" value="Add" />
            </table>
          </xml:group>
        )
      }

    </lift:surround>
  }
}
