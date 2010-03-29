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

package org.apache.esme.model

import net.liftweb._
import mapper._
import openid._
import util._
import common._
import Helpers._
import http._

import net.liftweb._
import http._
import js._
import js.jquery._
import http.jquery._
import JqJsCmds._
import JsCmds._ 
import SHtml._
import JE._

import scala.xml._

import org.apache.esme.actor._

import org.openid4java.discovery.Identifier
import org.openid4java.consumer._
import org.openid4java.util._


/**
 * A table that keeps track of authtentications (username/pwd, openid, etc.) for
 * a given user
 */
class UserAuth extends LongKeyedMapper[UserAuth] with IdPK {
  def getSingleton = UserAuth

  object user extends MappedLongForeignKey(this, User)

  /**
   * The module that's used for authentication
   */
  object authType extends MappedString(this, 64)

  /**
   * The key for authentication on the given type (e.g., email address, open-id, etc.
   */
  object authKey extends MappedString(this, 2048)

  /**
   * Data that's specific to the authentication module (e.g., a hashed password
   */
  object authData extends MappedText(this)
}

object UserAuth extends UserAuth with LongKeyedMetaMapper[UserAuth] {
  override def dbIndexes = Index(authType, authKey) :: super.dbIndexes

  private var modules: Map[String, AuthModule] = Map()

  def register(module: AuthModule) {
    modules += (module.moduleName -> module)
    if (module.isDefault && defAuth.isEmpty) defAuth = Full(module)
    module.performInit()
  }

  def loginPresentation: List[NodeSeq] =
  modules.values.toList.flatMap(_.loginPresentation)
  
  private var defAuth: Box[AuthModule] = Empty

  def defaultAuthModule: AuthModule = defAuth.open_!
}

trait AuthModule {
  def loginPresentation: Box[NodeSeq]
  def signupPresentation: Box[NodeSeq] = Empty
  def moduleName: String
  def performInit(): Unit
  def isDefault = false
  def createHolder(): FieldSet
}

trait FieldSet {
  def toForm: NodeSeq
  def validate: List[FieldError]
  def save(user: User): Unit
}

object UserPwdAuthModule extends AuthModule {
  def loginPresentation: Box[NodeSeq] =
  TemplateFinder.findAnyTemplate("templates-hidden" :: "upw_login_form" :: Nil)

  def moduleName: String = "upw"

  def performInit(): Unit = {
    LiftRules.dispatch.append {
      case Req("authentication" :: "login" :: Nil, _, PostRequest) =>
        val from = S.referer openOr "/"

        (for {
            name <- S.param("username").map(_.trim.toLowerCase)
            pwd <- S.param("password").map(_.trim)
            user <- UserAuth.find(By(UserAuth.authKey, name),
                                  By(UserAuth.authType, moduleName)).flatMap(_.user.obj) or
            User.find(By(User.nickname, name))
            
            userAuth <- UserAuth.find(By(UserAuth.user, user), By(UserAuth.authType, moduleName))
            if authenticatePassword(userAuth.authData.is, pwd)
          } yield user) match {
          case Full(user) =>
            User.logUserIn(user)
            S.notice(S.?("base_user_msg_welcome", user.niceName))

          case _ =>
            S.error(S.?("base_user_err_unknown_creds"))
        }

        S.redirectTo(from)
    }
  }

  private def authenticatePassword(hashed: String, pwd: String): Boolean =
  tryo {
    val (salt :: hashedPw :: _) = hashed.roboSplit(";")
    md5(salt+pwd) == hashedPw
  } openOr false

  override def isDefault = true

  def createHolder(): FieldSet = new FieldSet {
    private var pwd1 = ""
    private var pwd2 = ""
    private var email = ""

    def toForm: NodeSeq =
    TemplateFinder.findAnyTemplate("templates-hidden" :: "upw_signup_form" :: Nil).map(
      xhtml =>
      bind("signup", xhtml,
           "email" -%> SHtml.text(email, s => email = s.trim.toLowerCase),
           "pwd1" -%> SHtml.password(pwd1, s => pwd1 = s.trim),
           "pwd2" -%> SHtml.password(pwd2, s => pwd2 = s.trim))
    ) openOr NodeSeq.Empty

    def validate: List[FieldError] = (
    if (MappedEmail.validEmailAddr_?(email)) Nil else {
      val msg = S.?("base_user_err_bad_email")
      S.error(msg)
      List(FieldError(new FieldIdentifier {
        override def uniqueFieldId: Box[String] = Full("email")
      }, Text(msg)))
    }
    ) ::: (
    if (pwd1 != pwd2) {
      val msg = S.?("base_user_err_mismatch_password")
      DisplayMessage("messages", <b>{msg}</b>,  3 seconds, 3 seconds); 
      S.error(msg)
      List(FieldError(new FieldIdentifier {
        override def uniqueFieldId: Box[String] = Full("pwd1")
      }, Text(msg)))
    } else if (pwd1.length < 6) {
      val msg = S.?("base_user_err_password_too_short")
      S.error(msg)
      List(FieldError(new FieldIdentifier {
        override def uniqueFieldId: Box[String] = Full("pwd1")
      }, Text(msg)))
    } else Nil)

    def save(user: User): Unit = {
      val salt = randomString(10)
      val md5 = Helpers.md5(salt + pwd1)
      UserAuth.create.user(user).authType(moduleName).authKey(email).authData(salt+";"+md5).save
    }
  }
}

object OpenIDAuthModule extends AuthModule {
  def loginPresentation: Box[NodeSeq] =
  TemplateFinder.findAnyTemplate("templates-hidden" :: "openid_login_form" :: Nil)

  def moduleName: String = "openid"

  def performInit(): Unit = {
    LiftRules.dispatch.append {
      case Req("open_id" :: "login" :: Nil, _, PostRequest) =>
        val from = S.referer openOr "/"

        def logUserIn(openid: Box[Identifier], fo: Box[VerificationResult], exp: Box[Exception]): LiftResponse = {
          (openid, exp) match {
            case (Full(OpenIDAuthModule(user)), _) =>
              User.logUserIn(user)
              S.notice(S.?("base_user_msg_welcome",user.niceName))

              Message.create.author(user.id).
              when(Helpers.timeNow.getTime).
              source("login").
              setTextAndTags(S.?("base_user_msg_login", user.nickname), Nil, Empty).
              foreach{ msg =>
                if (msg.save) {
                  Distributor ! Distributor.AddMessageToMailbox(user.id, msg, LoginReason(user.id))
                }
              }

              RedirectResponse(from, S responseCookies :_*)

            case (Full(id), _) =>
              S.error(S.?("base_user_err_openid_not_reg",id.getIdentifier()))
              RedirectResponse(from, S responseCookies :_*)


            case (_, Full(exp)) =>
              S.error(S.?("base_error_exception", exp.getMessage))
              RedirectResponse(from, S responseCookies :_*)

            case _ =>
              S.error(S.?("base_user_err_login", fo.map(_.getStatusMsg)))
              RedirectResponse(from, S responseCookies :_*)
          }


        }
    
        for {
          username <- S.param("open_id")
        }  ESMEOpenIDVendor.loginAndRedirect(username, logUserIn)


        S.redirectTo(from)
    }
  }

  def createHolder(): FieldSet = new FieldSet {
    def toForm: NodeSeq = NodeSeq.Empty
    def validate: List[FieldError] = Nil
    def save(user: User): Unit = {}
  }
  
  def findOrCreate(openId: String): User =
    UserAuth.find(By(UserAuth.authType, moduleName), By(UserAuth.authKey, openId)).flatMap(_.user.obj) match {
      case Full(user) => user
      case _ => val user = User.createAndPopulate.nickname(openId).saveMe
        UserAuth.create.authType(moduleName).user(user).authKey(openId).save
        User.logUserIn(user)
        user
  }

  def unapply(openId: Identifier): Option[User] =
  for {
    id <- tryo(openId.getIdentifier()).toOption
    userAuth <- UserAuth.find(By(UserAuth.authType, moduleName),
                              By(UserAuth.authKey, id))
    user <- userAuth.user.obj
  } yield user
}


object ESMEOpenIDVendor extends OpenIdVendor {
  type UserType = User
  type ConsumerType = ESMEOpenIDConsumer

  def logUserOut(): Unit = User.logUserOut()

  def currentUser = User.currentUser

  def postLogin(id: Box[Identifier],res: VerificationResult): Unit = {
    id match {
      case Full(OpenIDAuthModule(user)) =>
        // val user: User = OpenIDAuthModule.findOrCreate(id.getIdentifier())
        User.logUserIn(user)
        S.notice(S.?("base_user_msg_welcome",user.niceName))

      case Full(id) =>
        S.error(S.?("base_user_err_openid_not_reg",id.getIdentifier()))


      case _ =>
        logUserOut()
        S.error(S.?("base_user_err_no_auth"))
    }
  }

  def displayUser(in: User): NodeSeq = Text(S.?("base_user_msg_welcome",User.niceName))

  def createAConsumer = new ESMEOpenIDConsumer
}

class ESMEOpenIDConsumer extends OpenIdConsumer[User]
{
  override val manager = {

    Log.info("Proxy settings: " + Props.get("http.proxyHost", "[no host]")
             + ":" + Props.get("http.proxyPort", "[no port]"))

    for (host <- Props.get("http.proxyHost")){
      val proxyProps = new ProxyProperties()
      proxyProps.setProxyHostName(host)
      proxyProps.setProxyPort(Props.getInt("http.proxyPort", 80))
      HttpClientFactory.setProxyProperties(proxyProps)
    }
    new ConsumerManager
  }
}
