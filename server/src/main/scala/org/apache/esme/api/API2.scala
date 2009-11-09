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

import scala.xml.{NodeSeq, Text, Elem, XML, Node}

import scala.collection.mutable.ListBuffer
import java.util.logging._

object API2 extends ApiHelper {
  val logger: Logger = Logger.getLogger("org.apache.esme.api")

  def dispatch: LiftRules.DispatchPF = {
    case Req("api2" :: "session" :: Nil, _, GetRequest) => allSessions
    case Req("api2" :: "session" :: Nil, _, PostRequest) => addSession          
    case Req("api2" :: "session" :: Nil, _, DeleteRequest) => removeSession                      
	
    case Req("api2" :: "users" :: Nil, _, GetRequest) => allUsers
// Add a method to get detail for a specific user

// Document the fact that tag is no longer a parameter here
    case Req("api2" :: "user" :: "messages" :: Nil, _, GetRequest) => allUserMsgs
// Document the new method for getting messages belonging to a particular tag
    case Req("api2" :: "user" :: "messages" :: "tag" :: tag :: Nil, _, GetRequest)
  		    => () => allUserMsgs(tag)
// Possibly deprecate and move to api2/messages or api2/pools/poolName/messages
// Add back long-poll option
    case Req("api2" :: "user" :: "messages" :: Nil, _, PostRequest) => () => addMsg

    case Req("api2" :: "user" :: "followees" :: Nil, _, GetRequest) => allFollowees         
    case Req("api2" :: "user" :: "followees" :: Nil, _, PostRequest) => addFollowee
    case Req("api2" :: "user" :: "followees" :: userId :: Nil, _, DeleteRequest) 
			=> removeFollow(Box(List(userId)))

    case Req("api2" :: "user" :: "followers" :: Nil, _, GetRequest) => allFollowers         

    case Req("api2" :: "user" :: "tracks" :: Nil, _, GetRequest) => allTracking
    case Req("api2" :: "user" :: "tracks" :: Nil, _, PostRequest) => addTracking
// Add a method to get detail for a specific track (or messages for the track?)
    case Req("api2" :: "user" :: "tracks" :: trackId :: Nil, _, DeleteRequest) => () 
			=> removeTracking(Box(List(trackId)))

    case Req("api2" :: "user" :: "actions" :: Nil, _, GetRequest) => allActions 
    case Req("api2" :: "user" :: "actions" :: Nil, _, PostRequest) => addAction
// Add a method to get detail of a specific action
    case Req("api2" :: "user" :: "actions" :: actionId :: Nil, _, PutRequest) => () 
			=> changeAction(Box(List(actionId)))
    case Req("api2" :: "user" :: "actions" :: actionId :: Nil, _, DeleteRequest) => () 
			=> removeAction(Box(List(actionId)))
                                                                                
    case Req("api2" :: "pools" :: Nil, _, GetRequest) => allPools 
    case Req("api2" :: "pools" :: Nil, _, PostRequest) => () => addPool 
// Add a method to delete pool
// Add a method to get the detail for a pool
// Add a method to get the list of users in a pool
    case Req("api2" :: "pools" :: poolId :: "users" :: Nil, _, PostRequest) => () 
			=> addUserToPool(Box(List(poolId))) 
// Add a method to delete a user from a pool   
// Add a method to get the messages from a pool
// Add a method to post a new message to a pool
    
// Add a method to get list of conversations
    case Req("api2" :: "conversations" :: conversationId :: Nil, _, GetRequest) => () 
			=> getConversation(Box(List(conversationId)))
// Add a method to post a message to a conversation??                          
  }

  def allSessions(): LiftResponse = {
    val r: Box[NodeSeq] = 
		for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
    	yield { 
			<session>{userToXml(user)}</session>
		}

	r
  }      

  def addSession(): LiftResponse = {
    val r: Box[NodeSeq] = if (User.loggedIn_?) Empty else
    for (token <- S.param("token") ?~ S.?("base_rest_api_err_missing_param", "token");
         auth <- AuthToken.find(By(AuthToken.uniqueId, token))
         ?~ "Token not found";
         user <- auth.user.obj;
         session <- S.session
    ) yield {
      User.logUserIn(user)
      val myActor = buildActor(user.id)
      restActor(Full(myActor))
      <session>{userToXml(user)}</session>
    }

    r
  } 

  def removeSession(): LiftResponse = {
    User.logUserOut()
    true
  } 


  def allUsers(): LiftResponse = {      	
  	val users: NodeSeq = for (user <- User.findAll) yield userToXml(user)
    val r: Elem = <users>{users}</users>
	r
  }
       
  def allUserMsgs(): LiftResponse = {
    val r: Box[Node] = 
      for (user <- calcUser ?~  S.?("base_rest_api_err_param_not_found", "User");
        val lst = Mailbox.mostRecentMessagesFor(user.id, 40))
      yield {
		<messages>{lst.flatMap{ case (msg, why, _) => msg.toXml % why.attr}}</messages>
	  }

    r
  }

  def allUserMsgs(tag: String): LiftResponse = {
	val r: Box[Node] =
      for (tagName <- Box(List(tag));
        tag <- Tag.find(By(Tag.name, tagName)))
      yield {
	    <tag>
		  <name>{tag.name}</name>
		  <messages>{tag.findMessages.map(_.toXml)}</messages>
	    </tag>
	  }
	
	r  	
  } 

  def addMsg(): LiftResponse = {
// Should return the created message

    val r: Box[Boolean] =
    for (user <- calcUser.map(_.id.is) ?~ S.?("base_rest_api_err_param_not_found", "User");
         msg <- S.param("message") ?~ S.?("base_rest_api_err_missing_param", "message"))
    yield {
      val from: String = S.param("via") openOr "api"
      val pool = for (poolName <- S.param("pool");
                      p <- AccessPool.findPool(poolName,
                        S.param("realm") openOr AccessPool.Native)
                      ) yield p.id.is

      val xml: Box[Elem] = S.param("metadata").flatMap(md =>
        tryo(XML.loadString(md)))

      Distributor !
      Distributor.UserCreatedMessage(user, msg,
                                     Tag.split(S.param("tags")
                                               openOr ""),
                                     millis,
                                     xml,
                                     from,
                                     S.param("replyto").map(toLong),
                                     pool)
      true
    }
    r
  }      


  def allFollowees(): LiftResponse = {
    val followees: NodeSeq = calcUser.map(_.following)
		.map(_.map(userToXml(_)))
		.openOr(<no_followees/>)

    <followees>{followees}</followees>
  }         

  def addFollowee(): LiftResponse = {
    val r: Box[Node] =
    for (user <- User.currentUser;
         userName <- S.param("userId");
         other <- User.findFromWeb(userName)
    ) yield { 	
		user.follow(other)
		userToXml(other)
    }
    
	r
  }   

  
  def removeFollow(userName: Box[String])(): LiftResponse = {
    val r: Box[Boolean] =
    for (user <- User.currentUser;
         userName <- userName;
         other <- User.findFromWeb(userName)
    ) yield user.unfollow(other)
    
    r
  }

  def allFollowers(): LiftResponse = {
  	val followers: NodeSeq = calcUser.map(_.followers)
		.map(_.map(userToXml(_)))
		.openOr(<no_followers/>)

  	<followers>{followers}</followers>
  }     

  def allTracking(): LiftResponse = {
    val tracks: Box[NodeSeq] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
    yield Tracking.findAll(By(Tracking.user, user)).flatMap(_.toXml)
    
	<tracks>{tracks}</tracks>
  }   

  def addTracking(): LiftResponse = {
    val ret: Box[Boolean] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
         toTrack <- (S.param("track") ?~ S.?("base_rest_api_err_missing_param", "track")) if toTrack.trim.length > 0)
    yield
    Tracking.create.user(user).regex(toTrack).save

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

  def allActions(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
    yield user.performing.flatMap(_.toXml)

    ret
  }     

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

  def changeAction(actionId: Box[String]): LiftResponse = {
    val ret: Box[Boolean] =
    for (action <- findAction(actionId);
         enabled <- S.param("enabled").map(toBoolean) ?~ S.?("base_rest_api_err_missing_param", "enable"))
    yield action.disabled(!enabled).save
  
    ret
  }

  def removeAction(actionId: Box[String]): LiftResponse = {
    val ret: Box[Boolean] =
    for (action <- findAction(actionId)) 
    yield action.removed(true).save
    
    ret
  }              

  def allPools(): LiftResponse = {
    val ret: Box[NodeSeq] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
    yield AccessPool.findAll(In(AccessPool.id, Privilege.pool, By(Privilege.user, user)),
                             OrderBy(AccessPool.id, Descending),
                             MaxRows(20)).
          flatMap(_.toXml)
    ret
  }      

  def addPool(): LiftResponse = {
    val r: Box[Boolean] =
    for (user <- User.currentUser;
         pool <- AccessPool.create.realm(AccessPool.Native).setName(S.param("poolName").openOr(""));
         privilegeSaved = Privilege.create.pool(pool.saveMe).user(user).
           permission(Permission.Admin).save
    ) yield {
      if (privilegeSaved) Distributor ! Distributor.AllowUserInPool(user.id.is, pool.id.is)
      privilegeSaved
    }
    
    r
  } 

  def addUserToPool(poolId: Box[String]): LiftResponse = {
    val r: Box[Boolean] = 
    for (adminUser <- User.currentUser;
         poolName <- poolId ?~ S.?("base_rest_api_err_missing_param", "pool");
         realm <- (S.param("realm") or Full(AccessPool.Native));
         pool <- AccessPool.findPool(poolName, realm) ?~  S.?("base_rest_api_err_param_not_found", "Pool");
         userName <- S.param("userId") ?~ S.?("base_rest_api_err_missing_param", "user");
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

  private def findAction(actionId: Box[String]): Box[Action] =
  	for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
         id <- actionId ?~ S.?("base_rest_api_err_missing_param", "id");
         action <- Action.find(By(Action.user, user),
                             By(Action.id, id.toLong),
                             By(Action.removed, false))) yield action

  
  private def calcUser: Box[User] =
  	S.param("user").flatMap(User.findFromWeb) or
  	User.currentUser 

  def createTag(in: NodeSeq) = <api_response>{in}</api_response>

  private def userToXml(user: User): Elem = 
	<user><id>{user.id.toString}</id><nickname>{user.niceName}</nickname><image>{user.image}</image><whole_name>{user.wholeName}</whole_name></user>
  
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

// TODO:
// 1. Get rid of calcUser and replace with User.currentUser ?~ S.?("base_rest_api_err_not_logged_in")
// 2. Fix errors so that they properly indicate a missing parameter or 404
// 3. Change changeAction so that if the "enabled" parameter doesn't show up it will simply use
//    the current value for the action, not throw an error.
// 4. Match based on the return content type header to determine what to return (default to XML)    
// 5. Re-enable streaming message API (using comet approach)

