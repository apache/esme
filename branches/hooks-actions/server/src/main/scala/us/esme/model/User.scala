package us.esme.model
/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenseUsers/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */


import net.liftweb._
import mapper._
import sitemap._
import util._
import openid._
import http._
import js._
import JsCmds._
import Helpers._
import sitemap._
import Loc._

import org.openid4java.discovery.Identifier
import org.openid4java.consumer._
import org.openid4java.util._

import scala.xml.{NodeSeq, Text}

import us.esme._
import actor._
import java.net.URL
import java.util.logging._

object User extends User with MetaOpenIDProtoUser[User] {
  val logger: Logger = Logger.getLogger("us.esme.model.User")
  logger.setLevel(Level.INFO)

  override def afterSave = notifyActors _ :: super.afterSave

  private def notifyActors(in: User) {
    Distributor ! Distributor.UserUpdated(in.id)
  }


  def findFromWeb(uid: String): Box[User] = 
  User.find(By(User.nickname, uid)) or User.find(uid)

  override def dbTableName = "users" // define the DB table name
  
  override def screenWrap = S.request.flatMap(_.location) match {
    case Full(l) if l.name == "Login" => Full(<lift:surround with="login" at="content">
          <lift:bind /></lift:surround>)
    case _ => Full(<lift:surround with="default" at="content">
          <lift:bind /></lift:surround>)
  }

  /**
   * The menu item for editing the user (make this "Empty" to disable)
   */
  override def editUserMenuLoc: Box[Menu] =
  Full(Menu(Loc("EditUser", editPath, "Profile",
                Template(() => wrapIt(editFunc.map(_()) openOr edit)),
                testLogginIn)))

  
  
  override def signupFields: List[BaseOwnedMappedField[User]] = nickname ::
  firstName :: lastName :: imageUrl :: timezone :: locale :: Nil
  
  override def fieldOrder: List[BaseOwnedMappedField[User]] = nickname ::
  firstName :: lastName :: imageUrl :: timezone :: locale :: Nil

  onLogIn = ExtSession.userDidLogin _ :: onLogIn

  onLogOut =  ExtSession.userDidLogout _ :: onLogOut
  
  override def loginXhtml =
  <form id="openid_submit" class="clear" method="POST" action={loginPath.mkString("/", "/", "")} >
    <div class="b-open-l">
      <p class="input"><label>Open ID</label><user:openid /></p>
      <p class="button">
        <img onclick="document.getElementById('openid_submit').submit()" src="/images/sign-on.png" alt="Sign On" />
      </p>
    </div>
    {
      if (openIdError.is)
      <div class="b-open-r">
        <h3>Oops!</h3>
        <p>We are not able validate your ID. Please try again.</p>
      </div>
      else Text("")
    }
  </form>

  object openIdError extends RequestVar(false)
  
  override def login = {
    if (S.post_?) {
      S.param("username").
      foreach(username => 
        ESMEOpenIDVendor.loginAndRedirect(username, logUserIn)
      )
    }
    
    def logUserIn(openid: Box[Identifier], fo: Box[VerificationResult], exp: Box[Exception]): LiftResponse = {
      (openid, exp) match {
        case (Full(id), _) =>
          val user = User.findOrCreate(id.getIdentifier)
          User.logUserIn(user)
          S.notice("Welcome "+user.niceName)
          RedirectResponse("/", S responseCookies :_*)
          
        case (_, Full(exp)) =>
          openIdError(true)
          S.error("Got an exception: "+exp.getMessage)
          RedirectResponse(S.uri, S responseCookies :_*)

        case _ =>
          openIdError(true)
          S.error("Unable to log you in: "+fo.map(_.getStatusMsg))
          RedirectResponse(S.uri, S responseCookies :_*)
      }

      
    }
    
    loginForm
  }
  
  def loginForm =     bind("user", loginXhtml,
                           "openid" -> (FocusOnLoad(<input type="text" name="username"/>)))
  
  def openIDVendor = ESMEOpenIDVendor
  
  override def logout = {
    logoutCurrentUser
    S.redirectTo("/static/about")
  }

  def followerIdsForUserId(userId: Long): List[Long] =
  Relationship.findAll(By(Relationship.target, userId)).map(_.owner.is)
}

object ESMEOpenIDVendor extends OpenIdVendor {
  type UserType = User
  type ConsumerType = ESMEOpenIDConsumer

  def logUserOut(): Unit = User.logUserOut()
  
  def currentUser = User.currentUser
  
  def postLogin(id: Box[Identifier],res: VerificationResult): Unit = {
    id match {
      case Full(id) =>
        val user = User.findOrCreate(id.getIdentifier())
        User.logUserIn(user)
        S.notice("Welcome "+user.niceName)

      case _ =>
        logUserOut()
        S.error("Failed to authenticate")
    }
  }
  
  def displayUser(in: User): NodeSeq = Text("Welcome "+in.niceName)
  
  def createAConsumer = new ESMEOpenIDConsumer
}

class ESMEOpenIDConsumer extends OpenIDConsumer[User]
{
  override val manager = {

    User.logger.info("Proxy settings: " + Props.get("http.proxyHost", "[no host]")
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

/**
 * An O-R mapped "User" class that includes first name, last name, password
 */
class User extends OpenIDProtoUser[User] {
  def getSingleton = User // what's the "meta" server

  object imageUrl extends MappedString(this, 256)
  
  def authTokens: List[AuthToken] =
  AuthToken.findAll(By(AuthToken. user, this),
                    OrderBy(AuthToken.description, Ascending))


  override lazy val toXml =
  <user id={id.toString} nickname={niceName} image={image} whole_name={wholeName} />

  def follow(who: User): Boolean = {
    if (who == this) false
    else
    Relationship.find(By(Relationship.owner, this),
                      By(Relationship.target, who)) match {
      case Full(x) => true
      case Empty => Relationship.create.owner(this).
        target(who).save
      case _ => false
    }
  }

  def unfollow(who: User): Boolean = {
    Relationship.findAll(By(Relationship.owner, this),
                         By(Relationship.target, who)).foreach(_.delete_!)
    true
  }

  def following_?(who: User): Boolean = 
  Relationship.find(By(Relationship.owner, this),
                    By(Relationship.target, who)).isDefined

  def following(): List[User] =
  User.findAll(In.fk(Relationship.target, By(Relationship.owner, this)))

  def followers(): List[User] =
  User.findAll(In.fk(Relationship.owner, By(Relationship.target, this)))
  
  def wholeName: String = (firstName.is, lastName.is) match {
    case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
    case (f, _) if f.length > 1 => f
    case (_, l) if l.length > 1 => l
    case (_, _) => niceName
  }

  def needsChange_? : Boolean = this.nickname.is.startsWith("chang") &&
  this.firstName.startsWith("Unkn") && this.lastName.startsWith("Unkn")

  def image: Option[NodeSeq] = tryo(Text((new URL(imageUrl)).toString)).toOption

  def tracking: List[Tracking] = 
  Tracking.findAll(By(Tracking.user, this),
                   By(Tracking.disabled, false),
                   By(Tracking.removed, false),
                   OrderBy(Tracking.id, Ascending))
  
  def performing: List[Action] =
  Action.findAll(By(Action.user, this),
                 By(Action.disabled, false),
                 By(Action.removed, false),
                 OrderBy(Action.id, Ascending))

}
