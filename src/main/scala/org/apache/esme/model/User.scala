package org.apache.esme.model
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

import net.liftweb._
import mapper._
import util.Mailer.From
import scala.xml.{Node, Elem, NodeSeq, Text}

import sitemap._
import util._
import common._
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

import org.apache.esme._
import org.apache.esme.actor._
import java.net.URL
import java.util.logging._
//import com.twitter.stats.Stats
import com.twitter.ostrich.stats.Stats

object User extends User with KeyedMetaMapper[Long, User] with Loggable {
  override def afterSave = profileChanged _ :: notifyActors _ :: super.afterSave

  override def afterCreate = createCryptoRecord _ :: super.afterCreate

  private def notifyActors(in: User) {
    Distributor ! Distributor.UserUpdated(in.id)
  }

  private def profileChanged(in: User) {
    if (!in.needsChange_?)
    Message.create.author(in.id).
    when(Helpers.timeNow.getTime).
    source("profile").
    
    setTextAndTags(S.?("base_user_msg_change", in.nickname, in.wholeName, in.imageUrl), Nil, Empty).
     foreach{ msg =>
      if (msg.save) {
        Distributor ! Distributor.AddMessageToMailbox(in.id, msg, ProfileReason(in.id))
      }
    }
  }

  private def createCryptoRecord(user: User) {
    UserCryptoSig.create.user(user).save
  }

  def findFromWeb(uid: String): Box[User] =
  User.find(By(User.nickname, uid)) or User.find(uid)

  override def dbTableName = "users" // define the DB table name

  def signupForm(xhtml: NodeSeq): NodeSeq = {
    val from = S.referer openOr "/"
    val user = User.create
    val auth = UserAuth.defaultAuthModule.createHolder()
    val snippetName = S.invokedAs


    def processEntryAdd() {
        logger.debug("processEntryAdd: " + firstName + ", " + lastName)
    }



    def doSubmit() {
      S.mapSnippet(snippetName, genForm)
      user.validate ::: auth.validate match {
        case Nil =>
          user.save
          auth.save(user)
          User.logUserIn(user)
          S.notice(S.?("base_user_msg_welcome", user.niceName))
          S.redirectTo(from)

        case fe =>
          S.error(fe)
      }
    }

    def genForm(xhtml: NodeSeq): NodeSeq = {
      bind("signup", xhtml,
           "nickname" -%> user.nickname.toForm,
           "firstname" -%> user.firstName.toForm,
           "lastname" -%> user.lastName.toForm,
           "image_url" -%> user.imageUrl.toForm,
           "timezone" -%> user.timezone.toForm,
           "locale" -%> user.locale.toForm,
           "credentials" -> auth.toForm,
           "submit" -%> SHtml.submit(S.?("base_user_ui_signup"), doSubmit _)
    ) ++ SHtml.hidden(() => { doSubmit})
    }


    genForm(xhtml)
  }


  def followerIdsForUserId(userId: Long): List[Long] =
  Relationship.findAll(By(Relationship.target, userId)).map(_.owner.is)

  /**
   * Create and populate a new user
   */
  def createAndPopulate: User = {
    val user = this.create.
    nickname("change"+Helpers.randomInt(1000000000)).firstName("Unknown").
    lastName("Unknown").
    // email(Helpers.randomInt(100000000)+"unknown@unknown.com").
    saveMe
    // create a crypto record for the user
    UserCryptoSig.create.user(user).save
    user
  }
  
  def findByNickname(str: String): List[User] =
  findAll(By(nickname, str))

  var onLogIn: List[User => Unit] = List(ExtSession.userDidLogin _)

  var onLogOut: List[Box[User] => Unit] = List(ExtSession.userDidLogout _)

  def loggedIn_? : Boolean = currentUserId.isDefined
  
  def logUserIdIn(id: String) {
    curUser.remove()
    curUserId(Full(id))
  }
  def logUserIn(who: User) {
    curUser.remove()
    curUserId(Full(who.id.toString))
    onLogIn.foreach(_(who))
    
    Message.create.author(who.id).
    when(Helpers.timeNow.getTime).
    source("login").
    setTextAndTags(S.?("base_user_msg_login", who.nickname), Nil, Empty).
    foreach{ msg =>
      if (msg.save) {
        Distributor ! Distributor.AddMessageToMailbox(who.id, msg, LoginReason(who.id))
       }
    }


    Stats incr "usersLoggedIn"
  }

  def logoutCurrentUser = logUserOut()

  def logUserOut() {
    onLogOut.foreach(_(curUser))
    curUserId.remove()
    curUser.remove()
    S.request.foreach(_.request.session.terminate)
    //Stats getCounter "usersLoggedIn" incr -1
    Stats incr ("usersLoggedIn", -1)
  }

  private object curUserId extends SessionVar[Box[String]](Empty)

  def currentUserId: Box[String] = curUserId.is

  private object curUser extends RequestVar[Box[User]](currentUserId.flatMap(id => getSingleton.find(id)))
  
  private object currentRole extends SessionVar[Box[String]](currentUser.flatMap(u => Props.get("role."+u.niceName)))

  def setRole(role : String) = {
    logger.debug("User.setRole() is being called. Role: '%s'".format(role))
    currentRole.set(Full(role))
  }

  def checkRole(role: String): Boolean = {
    val userRole:String = currentRole.openOr("")
    logger.debug("User.checkRole() is being called. Role to check: '%s'. Current role: '%s'".format(role, userRole))
    userRole.equals(role)
  }

  def currentUser: Box[User] = curUser.is

  def getNickname(userId: Long) = {
    User.find(userId) match {
      case Full(u : User) => u.nickname.is
      case _ => "ERROR"
    }
  }
}

/**
 * An O-R mapped "User" class that includes first name, last name, password
 */
class User extends KeyedMapper[Long, User] with UserIdAsString with ManyToMany {// OpenIDProtoUser[User] {
  import S._

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
      case Empty =>
        if (Relationship.create.owner(this).target(who).save)
        Message.create.author(who.id).
        when(Helpers.timeNow.getTime).
        source("followed").
        setTextAndTags("User @" + this.nickname + " followed @" + who.nickname, Nil, Empty).
        foreach { msg =>
          if (msg.save) {
            Distributor ! Distributor.AddMessageToMailbox(who.id, msg, FollowedReason(this.id))
          }
        }
        true
        
      case _ => false
    }
  }

  def unfollow(who: User): Boolean = {
    Relationship.findAll(By(Relationship.owner, this),
                         By(Relationship.target, who)).foreach{ r =>
      if (r.delete_!) Message.create.author(who.id).
      when(Helpers.timeNow.getTime).
      source("unfollowed").
      setTextAndTags("User @" + this.nickname + " unfollowed @" + who.nickname, Nil, Empty).
      foreach{ msg =>
        if (msg.save) {
          Distributor ! Distributor.AddMessageToMailbox(who.id, msg, UnfollowedReason(this.id))
        }
      }
    }
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
  
  /**
   * URL to the image that the user has provided 
   */
   
  
  def image_url: String = (imageUrl.is) match {
    case (f) if f.length > 1 => f
    case (_) => "/images/avatar.jpg" 
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
                 
  def performingwithdisabled: List[Action] =
  Action.findAll(By(Action.user, this),
                 By(Action.removed, false),
                 OrderBy(Action.id, Ascending))


  override def primaryKeyField = id

  // the primary key for the database
  object id extends MappedLongIndex(this)

  def userIdAsString: String = id.is.toString

  // First Name
  object firstName extends MappedString(this, 32) {
    override def displayName = fieldOwner.firstNameDisplayName
    override val fieldId = Some(Text("txtFirstName"))
  }

  def firstNameDisplayName = S.?("base_user_ui_first_name")

  // Last Name
  object lastName extends MappedString(this, 32) {
    override def displayName = fieldOwner.lastNameDisplayName
    override val fieldId = Some(Text("txtLastName"))
  }

  def lastNameDisplayName = S.?("base_user_ui_last_name")

  def emailDisplayName = S.?("base_user_ui_email")
  // Password

  object superUser extends MappedBoolean(this) {
    override def defaultValue = false
  }

  def shortName: String = (firstName.is, lastName.is) match {
    case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
    case (f, _) if f.length > 1 => f
    case (_, l) if l.length > 1 => l
    case _ => nickname // email.is
  }

   def image_link = <img src={""+image_url} width="30px" /> 

  object uniqueId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
    override def writePermission_?  = true
  }

  object validated extends MappedBoolean[User](this) {
    override def defaultValue = false
    override val fieldId = Some(Text("txtValidated"))
  }

  object locale extends MappedLocale[User](this) {
    override def displayName = fieldOwner.localeDisplayName
    override val fieldId = Some(Text("txtLocale"))
  }

  object timezone extends MappedTimeZone[User](this) {
    override def displayName = fieldOwner.timezoneDisplayName
    override val fieldId = Some(Text("txtTimeZone"))
  }


  object nickname extends MappedPoliteString(this, 64) {
    override def dbIndexed_? = true

    def deDupUnderscore(in: String): String = in.indexOf("__") match {
      case -1 => in
      case pos => deDupUnderscore(in.substring(0, pos)+in.substring(pos + 1))
    }

    override def setFilter = notNull _ :: toLower _ :: trim _ ::
    deDupUnderscore _ :: super.setFilter

    private def validateNickname(str: String): List[FieldError] = {
      val others = getSingleton.findByNickname(str).
      // getSingleton.findAll(By(getSingleton.nickname, str)).
      filter(_.id.is != fieldOwner.id.is)
      others.map{u =>
        val msg = S.?("base_user_err_duplicate_nickname", str)
        S.error(msg)
        FieldError(this, Text(msg))
      }
    }

    private def validText(str: String): List[FieldError] =
    if (ValidNickName(str)) Nil
    else List(FieldError(this,
                         <xml:group>Invalid nickname.  Must start with
          a letter and contain only letters,
          numbers or "_"</xml:group>))

    override def validations = validText _ :: validateNickname _ :: super.validations
  }

  def niceName: String = nickname

  def timezoneDisplayName = S.?("base_user_ui_timezone")

  def localeDisplayName = S.?("base_user_ui_locale")  
  
  object tagsfollowing extends MappedManyToMany(UserTagFollow, UserTagFollow.user, UserTagFollow.tag, Tag)
  
  object convsfollowing extends MappedManyToMany(UserConvFollow, UserConvFollow.user, UserConvFollow.conversation, Message) 
}
