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

import org.openid4java.discovery.Identifier
import org.openid4java.consumer._

import scala.xml._

/**
 * Manage the sitemap and related snippets for editing a user's profile
 */
object ProfileMgr {
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("base_error_not_logged_in")))

  val menuItems =
  Menu(Loc("profile", List("profile_view", "edit"), S.?("base_profile_menu"), ifIsLoggedIn,
           Loc.Snippet("editProfile", editProfile))) ::
  Nil

  def editProfile(in: NodeSeq): NodeSeq = {
    import OpenIDAuthModule.moduleName

    (for (user <- User.currentUser) yield {
      
      val openID = UserAuth.find(By(UserAuth.user, user),
                                 By(UserAuth.authType, moduleName))
                                 
      val openIdUrl = openID.map(_.authKey.is) getOrElse ""
      val from = "/profile_view/edit"
      
      def saveOpenID(openid: Box[Identifier], fo: Box[VerificationResult], exp: Box[Exception]): LiftResponse = {
        (openid, exp) match {
          case (Full(id), _) =>
            UserAuth.create.authType(moduleName).user(user).authKey(id.getIdentifier()).save
        
          case (_, Full(exp)) =>
            S.error(S.?("base_error_exception", exp.getMessage))

          case _ =>
            S.error(S.?("base_user_err_login", fo.map(_.getStatusMsg)))
        }
        RedirectResponse(from, S responseCookies :_*)
      }
          
      def registerOpenID (url: String) {
        if (openIdUrl != url) {
          if (url != "") {
            val other = UserAuth.find(NotBy(UserAuth.user, user),
                                      By(UserAuth.authType, moduleName),
                                      By(UserAuth.authKey, url))
            other match {
              case Empty =>
                ESMEOpenIDVendor.loginAndRedirect(url, saveOpenID)
              // TODO: localize
              case _ => S.error("This OpenID URL is registered with another user!")
            }
          } else {
            for (auth <- openID) auth.delete_!
          }
        }
      }
      
      bind("user", in, "nickname" -> text(user.nickname, {_ =>}, "disabled" -> "true"),
                       "lastName" -> user.lastName.toForm,
                       "imageURL" -> user.imageUrl.toForm,
                       "firstName" -> user.firstName.toForm,
                       "timezone" -> user.timezone.toForm,
                       "locale" -> user.locale.toForm,
                       "openid" -> text(openIdUrl, registerOpenID(_)),
                       "save" -> submit("Save", user.save))
    }).getOrElse(NodeSeq.Empty)

  }

}
