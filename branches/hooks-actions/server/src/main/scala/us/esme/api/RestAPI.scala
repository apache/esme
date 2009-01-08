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
/*
 * RestAPI.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package us.esme.api

import net.liftweb._
import http._
import rest._
import util._
import mapper._
import Helpers._

import us.esme._
import model._
import actor._

import scala.xml.{NodeSeq, Text, Elem, XML}
import scala.actors.Actor
import Actor._

import scala.collection.mutable.ListBuffer
import java.util.logging._

object RestAPI extends XMLApiHelper {
  val logger: Logger = Logger.getLogger("us.esme.api")

  def dispatch: LiftRules.DispatchPF = {
    case Req("api" :: "status" :: Nil, "", GetRequest) => status
    case Req("api" :: "login" :: Nil, "", PostRequest) => login
    case Req("api" :: "logout" :: Nil, "", GetRequest) => logout
    case Req("api" :: "get_msgs" :: Nil, "", GetRequest) => getMsgs
    case Req("api" :: "wait_for_msgs" :: Nil, "", GetRequest) =>
      waitForMsgs
      
    case Req("api" :: "send_msg" :: Nil, "", PostRequest) => 
      () => sendMsg(User.currentUser.map(_.id.is), S)

    case Req("api" :: "get_following" :: Nil, _, GetRequest) =>
      following(calcUser)
    case Req("api" :: "get_followers" :: Nil, _, GetRequest) =>
      followers(calcUser)
    case Req("api" :: "follow" :: Nil, _, PostRequest) =>
      performFollow(S.param("user"))
    case Req("api" :: "unfollow" :: Nil, _, PostRequest) => 
      performUnfollow(S.param("user"))
    case Req("api" :: "all_users" ::
                      Nil, _, GetRequest) => allUsers _
    case Req("api" :: "get_tagcloud" :: Nil, "", GetRequest) =>
      () => getTagCloud(S)

      // DPP to document
    case Req("api" :: "get_tracking" :: Nil, "", GetRequest) =>
      getTracking

    case Req("api" :: "add_tracking" :: Nil, "", PostRequest) =>
      addTracking

    case Req("api" :: "remove_tracking" :: Nil, "", PostRequest) =>
      removeTracking

    case Req("api" :: "get_conversation" :: Nil, "", GetRequest) =>
      getConversation
      
    case Req("api" :: "get_actions" :: Nil, "", GetRequest) =>
      getActions _
        
    case Req("api" :: "add_action" :: Nil, "", PostRequest) =>
      addAction _
        
    case Req("api" :: "enable_action" :: Nil, "", PostRequest) =>
      enableAction _
        
    case Req("api" :: "delete_action" :: Nil, "", PostRequest) =>
      deleteAction _
  }

  def findAction: Box[Action] =
  for (user <- User.currentUser ?~ "Not Logged In";
       id <- S.param("actionid") ?~ "id param not supplied";
       action <- Action.find(By(Action.user, user),
                             By(Action.id, id.toLong),
                             By(Action.removed, false))) yield action

  def addAction(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ "Not logged in";
         name <- S.param("name") ?~ "'name' param not supplied";
         test <- S.param("test") ?~ "'test' param not supplied";
         action <- S.param("action") ?~ "'action' param not supplied";
         val a = Action.create.user(user).name(name);
         a2 <- a.setTest(test);
         a3 <- a.setAction(action)) yield a3.saveMe.toXml

    ret
  }

  def getActions(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ "Not logged in")
    yield user.performing.flatMap(_.toXml)

    ret
  }

  def enableAction(): LiftResponse = {
    val ret: Box[Boolean] =
    for (action <- findAction;
         enabled <- S.param("enabled").map(toBoolean) ?~ "enabled param not supplied")
    yield action.disabled(!enabled).save
  
    ret
  }

  def deleteAction(): LiftResponse = {
    val ret: Box[Boolean] =
    for (action <- findAction) 
    yield action.removed(true).save
    
    ret
  }
  
  private def calcUser: Box[User] =
  S.param("user").flatMap(User.findFromWeb) or
  User.currentUser
  
  def getTracking(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ "Not logged in")
    yield Tracking.findAll(By(Tracking.user, user)).flatMap(_.toXml)
    ret
  }

  def getConversation(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ "Not logged in";
         id <- S.param("conversationid").map(toLong) ?~ "id param missing"
    ) yield <conversation id={id.toString}>{
        Message.findAndPrime(By(Message.conversation, id),
                             OrderBy(Message.id, Ascending)).map(_.toXml)
      }</conversation>

    ret
  }

  def removeTracking(): LiftResponse = {
    val ret: Box[Boolean] =
    for (user <- User.currentUser ?~ "Not logged in";
         id <- S.param("trackid") ?~ "id param missing";
         track <- Tracking.find(By(Tracking.id, id.toLong),
                                By(Tracking.user, user)) ?~ "Couldn't find tracking item"
    ) yield track.removed(true).save

    ret
  }

  def addTracking(): LiftResponse = {
    val ret: Box[Boolean] =
    for (user <- User.currentUser ?~ "Not logged in";
         toTrack <- (S.param("track") ?~ "No track param") if toTrack.trim.length > 0)
    yield
    Tracking.create.user(user).regex(toTrack).save

    ret
  }

  def status(): LiftResponse =
  {
    val ret: Box[NodeSeq] = User.currentUser.map(_.toXml)
    ret
  }

  def allUsers(): LiftResponse = 
  for (user <- User.findAll) yield user.toXml


  def following(muser: Box[User])(): LiftResponse = {
    val r: Box[NodeSeq] = for (user <- muser) yield
    user.following().map(_.toXml)
    
    r
  }
  
  def followers(muser: Box[User])(): LiftResponse = {
    val r: Box[NodeSeq] = for (user <- muser) yield
    user.followers().map(_.toXml)
    
    r
  }
  
  def performFollow(userName: Box[String])(): LiftResponse = {
    val r: Box[Boolean] =
    for (user <- User.currentUser;
         userName <- userName;
         other <- User.findFromWeb(userName)
    ) yield user.follow(other)
    
    r
  }
  
  def performUnfollow(userName: Box[String])(): LiftResponse = {
    val r: Box[Boolean] =
    for (user <- User.currentUser;
         userName <- userName;
         other <- User.findFromWeb(userName)
    ) yield user.unfollow(other)
    
    r
  }

  def login(): LiftResponse = {
    val res: Box[Boolean] = if (User.loggedIn_?) Empty else
    for (token <- S.param("token") ?~ "No 'token' param";
         auth <- AuthToken.find(By(AuthToken.uniqueId, token))
         ?~ "Token not found";
         user <- auth.user.obj;
         session <- S.session
    ) yield {
      User.logUserIn(user)
      val myActor = buildActor(user.id)
      restActor(Full(myActor))
      true
    }

    res
  }

  def logout(): LiftResponse = {
    User.logUserOut()
    true
  }

  def waitForMsgs(): LiftResponse = {
    val seq: Long = Helpers.nextNum

    def waitForAnswer: Box[List[(Message, MailboxReason)]] = 
    receiveWithin(6L * 60L * 1000L) {
      case (s2, ret: List[(Message, MailboxReason)]) if s2 == seq =>
        Full(ret)
      case (s2, _, _) if s2 != seq => waitForAnswer
      case _ => Empty
    }

    var r: Box[NodeSeq] = 
    for (act <- restActor.is ?~ "No REST actor";
         val ignore = act ! ListenFor(self, 5 minutes, seq);
         answer <- waitForAnswer ?~ "Didn't get an answer")
    yield answer.flatMap{ case (msg, reason) => msg.toXml % reason.attr}

    r
  }

  def sendMsgWithToken(req: Req): Box[LiftResponse] = {
    for (token <- req.param("token");
         auth <- AuthToken.find(By(AuthToken.uniqueId, token));
         userId <- auth.user.can;
         ret <- sendMsg(Full(userId), req))  yield ret
  }
  
  def sendMsg(theUser: Box[Long], params: HasParams): LiftResponse = {
    val r: Box[Boolean] =
    for (user <- theUser ?~ "User not found";
         msg <- params.param("message") ?~ "Message not included")
    yield {
      val from: String = params.param("via") openOr "api"

      val xml: Box[Elem] = params.param("metadata").flatMap(md =>
        tryo(XML.loadString(md)))

      Distributor !
      Distributor.UserCreatedMessage(user, msg,
                                     Tag.split(params.param("tags")
                                               openOr ""),
                                     millis,
                                     xml,
                                     from,
                                     params.param("replyto").map(toLong))
      true
    }
    r
  }

  def getMsgs(): LiftResponse = {
    val t: Box[NodeSeq] =
    for (tagName <- S.param("tag");
         tag <- Tag.find(By(Tag.name, tagName)))
    yield tag.findMessages.map(_.toXml)
        
    val r: Box[NodeSeq] = 
    t or (for (user <- calcUser ?~ "User not found";
               val lst = Mailbox.mostRecentMessagesFor(user.id, 40))
          yield lst.flatMap{ case (msg, why) => msg.toXml % why.attr})
    
    r
  }

  def getTagCloud(params: HasParams): LiftResponse = {
    // By default, get the normalised word frequencies from the messages
    val numTags = (params.param("numTags") openOr "20").toInt
    val numMsgs = 40 //(params.param("numMsgs") openOr "40").toInt

    val r: Box[NodeSeq] = 
    for (user <- calcUser ?~ "User not found";
         val lst = Mailbox.mostRecentMessagesFor(user.id, numMsgs).map(_._1))
    yield
    <tag_cloud>
      {
        for (t <- Tag.centreWeightedTopNTagFreqs(lst, numTags)) yield <tag name={t._1} weight={t._2.toString} />
      }
      {
        for (t <- Message.centreWeightedTopNWordFreqs(lst, numTags)) yield <word name={t._1} weight={t._2.toString} />
      }
    </tag_cloud>
     
     
    r
  }

  def createTag(in: NodeSeq) = <esme_api>{in}</esme_api>

  
  private def buildActor(userId: Long): RestActor = {
    val ret = new RestActor
    ret.start
    ret ! StartUp(userId)
    ret
  }

  object restActor extends SessionVar[Box[RestActor]](Empty) {
    override def cleanupFunc: Box[() => Unit] = Full(() => this.is.map(_ ! ByeBye))
  }
  

  class RestActor extends Actor {
    private var userId: Long = _
    private var msgs: List[(Message, MailboxReason)] = Nil
    private var listener: Box[(Actor, Long)] = Empty
    
    def act = loop {
      react {
        case StartUp(userId) =>
          link(ActorWatcher)
          this.userId = userId
          Distributor ! Distributor.Listen(userId, this)

        case ByeBye =>
          unlink(ActorWatcher)
          Distributor ! Distributor.Unlisten(userId, this)
          self.exit()
          
        case UserActor.MessageReceived(msg, reason) =>
          msgs = (msg, reason) :: msgs
          listener.foreach {
            who =>
            who._1 ! (who._2, msgs)
            listener = Empty
            msgs = Nil
          }
        
        case ReleaseListener =>
          listener.foreach(l => l._1 ! (l._2, Nil))
          listener = Empty

        case ListenFor(who, len, seq) =>
          msgs match {
            case Nil =>
              listener.foreach(l => l._1 ! (l._2, Nil))
              listener = Full((who, seq))
              ActorPing.schedule(this, ReleaseListener, len)
             
            case xs =>
              who ! (seq, xs)
              msgs = Nil
              listener = Empty
          }
      }
    }
  }

  private case class StartUp(userId: Long)
  private case object ByeBye
  private case class ListenFor(who: Actor, howLong: TimeSpan, seq: Long)
  private case object ReleaseListener
}
