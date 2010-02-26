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

import java.util.Date
import java.text.{DateFormat,SimpleDateFormat}

import scala.xml._

/**
 * Manage the sitemap and related snippets for Actions
 */
object ActionMgr {
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("base_menu_logout_error")))

  val menuItems =
  Menu(Loc("actionMgt", List("action_view", "index"), S.?("base_actions_menu"), ifIsLoggedIn,
           Loc.Snippet("displayActions", displayActions),
           Loc.Snippet("main", mainActions))) ::
  Nil

  object updateActions extends RequestVar[() => JsCmd](() => Noop)
  
      //XXX display date, should we have a common dateFormat?
    val dateFormat = new SimpleDateFormat("yyyy/MM/dd")
    def getDateHtml(date: Date) : Text = date match {
     case null => Text(S.?("base_pool_ui_empty_date"))
     case d => Text(dateFormat.format(d))
   }

  def displayActions(in: NodeSeq): NodeSeq = {
    // get the span name to update
    val spanName = S.attr("the_id") openOr "TokenSpan"
    // get the current user
    val user = User.currentUser

    // bind the dynamic content to the incoming nodeseq
    def doRender(): NodeSeq =
    Action.findAll(By(Action.user, user), By(Action.removed, false),
                   OrderBy(Action.id, Ascending)) match {
      case Nil => NodeSeq.Empty
      case xs => bind("disp", in,
                      "item" -> 
                      (lst => xs.flatMap(i => bind("item", lst,
                                                   "name" -> i.name.is,
                                                   "enabled" -> ajaxCheckbox(!i.disabled,
                                                                             e => {i.disabled(!e).save; S.notice(S.?("base_action_msg_active")); Noop} ),
                                                   "test" -> i.testText,
                                                   "action" -> i.actionText,
                                                   "createdDate" -> getDateHtml(i.createdDate),
                                                   "remove" -> 
                                                   ((bt: NodeSeq) => 
                  ajaxButton(bt, () => {i.removed(true).save ; S.notice(S.?("base_action_msg_removed", i.name.is)); updateSpan()}))
              ))))
    }

    def updateSpan(): JsCmd = SetHtml(spanName, doRender())

    updateActions.set(updateSpan)
    doRender()
  }

  def mainActions(in: NodeSeq): NodeSeq = {
    val redisplay = updateActions.is
    var name = ""
    var test = ""
    val mainName = "name_input"
    val mainTest = "test_input"
    val mainAction = "action_input"
    val user = User.currentUser

    def doSave(actionText: String): JsCmd = {
      val toSave = Action.create.name(name).user(user)
      DB.use(toSave.connectionIdentifier) {
        ignore =>
        val act: Box[List[FieldError]] =
        for (a1 <- toSave.setAction(actionText);
             a2 <- a1.setTest(test))
        yield a2.validate

        act match {
          case Full(Nil) =>
            toSave.save 
            S.notice(S.?("base_action_msg_new_action", name))
            redisplay() &
            SetValById(mainName, "") &
            SetValById(mainAction, "") &
            SetValById(mainTest, "")

          case Full(xs) => S.error(xs); Noop
          case Failure(msg, _, _) => S.error(msg) ; Noop
          case _ => Noop
        }
      }
    }
    

    bind("main", in,
         "name" -> text(name, name = _, "id" -> mainName),
         "test" -> textarea(test, test = _, "id" -> mainTest),
         "action" -> textarea("", doSave, "id" -> mainAction)
    )
  }
}
