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
 * API2.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.apache.esme.api

import net.liftweb._
import http._
import actor._
import rest._
import util._
import common._
import mapper._
import Helpers._

import org.apache.esme._
import model._
import org.apache.esme.actor._

import scala.xml.{NodeSeq, Text, Elem, XML}

import scala.collection.mutable.ListBuffer
import java.util.logging._

object API2 extends XMLApiHelper {
  val logger: Logger = Logger.getLogger("org.apache.esme.api")

  def dispatch: LiftRules.DispatchPF = {
    case Req("api2" :: "session" :: Nil, _, GetRequest) => status
    case Req("api2" :: "session" :: Nil, _, PostRequest) => login             
    case Req("api2" :: "session" :: Nil, _, DeleteRequest) => logout                       
	
    case Req("api2" :: "users" :: Nil, _, GetRequest) => allUsers _

    case Req("api2" :: "user" :: "messages" :: Nil, _, GetRequest) => getMsgs     
    case Req("api2" :: "user" :: "messages" :: Nil, _, PostRequest) => () => sendMsg(User.currentUser.map(_.id.is), S)

    case Req("api2" :: "user" :: "followees" :: Nil, _, GetRequest) => following(calcUser)            
    case Req("api2" :: "user" :: "followees" :: Nil, _, PostRequest) => performFollow(S.param("user"))
    case Req("api2" :: "user" :: "followees" :: userId :: Nil, _, DeleteRequest) => performUnfollow(Box(List(userId)))

    case Req("api2" :: "user" :: "followers" :: Nil, _, GetRequest) => followers(calcUser)             

    case Req("api2" :: "user" :: "tracks" :: Nil, _, GetRequest) => getTracking
    case Req("api2" :: "user" :: "tracks" :: Nil, _, PostRequest) => addTracking   
    case Req("api2" :: "user" :: "tracks" :: trackId :: Nil, _, DeleteRequest) => () => removeTracking(Box(List(trackId)))

    case Req("api2" :: "user" :: "actions" :: Nil, _, GetRequest) => getActions _        
    case Req("api2" :: "user" :: "actions" :: Nil, _, PostRequest) => addAction _     
    case Req("api2" :: "user" :: "actions" :: actionId :: Nil, _, PutRequest) => () => enableAction(Box(List(actionId)))      
    case Req("api2" :: "user" :: "actions" :: actionId :: Nil, _, DeleteRequest) => () => deleteAction(Box(List(actionId)))
                                                                                
    case Req("api2" :: "pools" :: Nil, _, GetRequest) => getPools
    case Req("api2" :: "pools" :: poolName :: Nil, _, PostRequest) => () => addPool(poolName)   
    case Req("api2" :: "pools" :: poolId :: "users" :: userId :: Nil, _, PostRequest) => () 
			=> addUserToPool(Box(List(userId)), Box(List(poolId)))

    case Req("api2" :: "conversations" :: conversationId :: Nil, _, GetRequest) => () 
			=> getConversation(Box(List(conversationId)))      
                                                                                   
// Do we need this?
    case Req("api2" :: "wait_for_msgs" :: Nil, _, GetRequest) => waitForMsgs
  }

  def findAction(actionId: Box[String]): Box[Action] =
  	for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
       id <- actionId ?~ S.?("base_rest_api_err_missing_param", "id");
       action <- Action.find(By(Action.user, user),
                             By(Action.id, id.toLong),
                             By(Action.removed, false))) yield action

  def addAction(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
         name <- S.param("name") ?~ S.?("base_rest_api_err_missing_param", "name");
         test <- S.param("test") ?~ S.?("base_rest_api_err_missing_param", "test");
         action <- S.param("action") ?~ S.?("base_rest_api_err_missing_param", "action");
         val a = Action.create.user(user).name(name);
         a2 <- a.setTest(test);
         a3 <- a.setAction(action)) yield a3.saveMe.toXml

    ret
  }

  def getActions(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
    yield user.performing.flatMap(_.toXml)

    ret
  }

  def enableAction(actionId: Box[String]): LiftResponse = {
    val ret: Box[Boolean] =
    for (action <- findAction(actionId);
         enabled <- S.param("enabled").map(toBoolean) ?~ S.?("base_rest_api_err_missing_param", "enable"))
    yield action.disabled(!enabled).save
  
    ret
  }

  def deleteAction(actionId: Box[String]): LiftResponse = {
    val ret: Box[Boolean] =
    for (action <- findAction(actionId)) 
    yield action.removed(true).save
    
    ret
  }
  
  private def calcUser: Box[User] =
  S.param("user").flatMap(User.findFromWeb) or
  User.currentUser
  
  def getTracking(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
    yield Tracking.findAll(By(Tracking.user, user)).flatMap(_.toXml)
    ret
  }

  def getConversation(conversationId: Box[String]): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
         id <- conversationId.map(toLong) ?~ S.?("base_rest_api_err_missing_param", "id")
    ) yield <conversation id={id.toString}>{
        Message.findAndPrime(By(Message.conversation, id),
                             OrderBy(Message.id, Ascending)).map(_.toXml)
      }</conversation>

    ret
  }

  def removeTracking(trackId: Box[String]): LiftResponse = {
    val ret: Box[Boolean] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
         id <- trackId ?~ S.?("base_rest_api_err_missing_param", "id");
         track <- Tracking.find(By(Tracking.id, id.toLong),
                                By(Tracking.user, user)) ?~ "Couldn't find tracking item"
    ) yield track.removed(true).save

    ret
  }

  def addTracking(): LiftResponse = {
    val ret: Box[Boolean] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
         toTrack <- (S.param("track") ?~ S.?("base_rest_api_err_missing_param", "track")) if toTrack.trim.length > 0)
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
    for (token <- S.param("token") ?~ S.?("base_rest_api_err_missing_param", "token");
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
    val future = new LAFuture[List[(Message, MailboxReason)]]()
    
    def waitForAnswer: Box[List[(Message, MailboxReason)]] = 
      future.get(6L * 60L * 1000L)

    var r: Box[NodeSeq] = 
    for (act <- restActor.is ?~ "No REST actor";
         val ignore = act ! ListenFor(future, 5 minutes);
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
    for (user <- theUser ?~ S.?("base_rest_api_err_param_not_found", "User");
         msg <- params.param("message") ?~ S.?("base_rest_api_err_missing_param", "message"))
    yield {
      val from: String = params.param("via") openOr "api"
      val pool = for (poolName <- params.param("pool");
                      p <- AccessPool.findPool(poolName,
                        params.param("realm") openOr AccessPool.Native)
                      ) yield p.id.is

      val xml: Box[Elem] = params.param("metadata").flatMap(md =>
        tryo(XML.loadString(md)))

      Distributor !
      Distributor.UserCreatedMessage(user, msg,
                                     Tag.split(params.param("tags")
                                               openOr ""),
                                     millis,
                                     xml,
                                     from,
                                     params.param("replyto").map(toLong),
                                     pool)
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
    t or (for (user <- calcUser ?~  S.?("base_rest_api_err_param_not_found", "User");
               val lst = Mailbox.mostRecentMessagesFor(user.id, 40))
          yield lst.flatMap{ case (msg, why, _) => msg.toXml % why.attr})
    
    r
  }

  def addPool(poolName: String): LiftResponse = {
    val r: Box[Boolean] =
    for (user <- User.currentUser;
         pool <- AccessPool.create.realm(AccessPool.Native).setName(poolName);
         privilegeSaved = Privilege.create.pool(pool.saveMe).user(user).
           permission(Permission.Admin).save
    ) yield {
      if (privilegeSaved) Distributor ! Distributor.AllowUserInPool(user.id.is, pool.id.is)
      privilegeSaved
    }
    
    r
  }
  
  def addUserToPool(userId: Box[String], poolId: Box[String]): LiftResponse = {
    val r: Box[Boolean] = 
    for (adminUser <- User.currentUser;
         poolName <- poolId ?~ S.?("base_rest_api_err_missing_param", "pool");
         realm <- (S.param("realm") or Full(AccessPool.Native));
         pool <- AccessPool.findPool(poolName, realm) ?~  S.?("base_rest_api_err_param_not_found", "Pool");
         userName <- userId ?~ S.?("base_rest_api_err_missing_param", "user");
         user <- User.findFromWeb(userName) ?~  S.?("base_rest_api_err_param_not_found", "User");
         permissionName <- (S.param("permission") or Full("Write"));
         permission <- Box(Permission.valueOf(permissionName)) ?~ S.?("base_rest_api_err_param_not_found", "Permission")
    ) yield if(Privilege.hasPermission(adminUser.id.is, pool.id.is, Permission.Admin)) {
      val result = try {
        Privilege.create.user(user).pool(pool).permission(permission).save
      } catch {
        case _: Exception => false
      }
      if (result) Distributor ! Distributor.AllowUserInPool(user.id.is, pool.id.is)
      result
    } else false // "User has no permission to administer pool"
    
    r
  }
  
  def getPools(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
    yield AccessPool.findAll(In(AccessPool.id, Privilege.pool, By(Privilege.user, user)),
                             OrderBy(AccessPool.id, Descending),
                             MaxRows(20)).
          flatMap(_.toXml)
    ret
  }

  def createTag(in: NodeSeq) = <esme_api>{in}</esme_api>

  
  private def buildActor(userId: Long): RestActor = {
    val ret = new RestActor
    ret ! StartUp(userId)
    ret
  }

  object restActor extends SessionVar[Box[RestActor]](Empty) {
    override def onShutdown(session: LiftSession) = this.is.map(_ ! ByeBye)
  }
  

  class RestActor extends LiftActor {
    private var userId: Long = _
    private var msgs: List[(Message, MailboxReason)] = Nil
    private var listener: Box[LAFuture[List[(Message, MailboxReason)]]] = Empty
    
    protected def messageHandler = {
      case StartUp(userId) =>
          this.userId = userId
          Distributor ! Distributor.Listen(userId, this)

        case ByeBye =>
          Distributor ! Distributor.Unlisten(userId, this)
          
      case UserActor.MessageReceived(msg, reason) =>
        msgs = (msg, reason) :: msgs
      listener.foreach {
        who =>
          who.satisfy(msgs)
        listener = Empty
        msgs = Nil
      }
      
      case ReleaseListener =>
        listener.foreach(_.satisfy(Nil))
      listener = Empty
      
      case ListenFor(who, len) =>
        msgs match {
            case Nil =>
              listener.foreach(_.satisfy(Nil))
              listener = Full(who)
              ActorPing.schedule(this, ReleaseListener, len)
             
            case xs =>
              who.satisfy(xs)
              msgs = Nil
              listener = Empty
        }
    }
  }


  private case class StartUp(userId: Long)
  private case object ByeBye
  private case class ListenFor(who: LAFuture[List[(Message, MailboxReason)]],
			       howLong: TimeSpan)
  private case object ReleaseListener
}
