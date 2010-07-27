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

/*
 * API2.scala
 *
 * The structure of the API2 object is a dispatch rule table,
 * which is a match against the Lift request object Req(). The match
 * determines a function to call. The convention in our case is that
 * the function has a return type of
 * Box[Tuple3[Int,Map[String,String],Box[Elem]]]
 *
 * This return type is inspired by Rack/WSGI and is implicitly
 * converted to the correct type of Lift Response by the ApiHelper
 * trait.
 *
 * The semantic structure of the response type
 * Box[Tuple3[Int,Map[String,String],Box[Elem]]]
 * is a Box (an Empty is converted to a 500 response) containing
 * a 3-tuple, containing in order:
 *   1. An Int representing the response code
 *   2. A Map(String,String) representing response headers
 *   3. A Box[Elem] containing the response body.
 *
 */

package org.apache.esme.api

import net.liftweb._
import http._
import auth._
import actor._
import rest._
import util._
import common._
import mapper._
import Helpers._

import org.apache.esme._
import model._
import org.apache.esme.actor._

import scala.xml._

import scala.collection.mutable.ListBuffer


import org.compass.annotations._
import bootstrap.liftweb.Compass.compass
import org.compass.core._
import lucene.util._
import org.apache.lucene.index.TermFreqVector
import org.tartarus.snowball.ext.PorterStemmer

object API2 extends ApiHelper with XmlHelper {
  val logger: Logger = Logger("org.apache.esme.api2")

  def authorizationRules: LiftRules.DispatchPF = {
    case Req("api2" :: "users" :: Nil, _, PostRequest)
      if !User.checkRole("integration-admin") => unAuthorized
    case Req("api2" :: "users" :: _ :: tokens :: Nil, _, GetRequest)
      if !User.checkRole("integration-admin") => unAuthorized
    case Req("api2" :: "users" :: _ :: tokens :: Nil, _, PostRequest)
      if !User.checkRole("integration-admin") => unAuthorized
    case Req("api2" :: "pools" :: poolId :: _, _, GetRequest)
      if !Privilege.hasPermission(
        User.currentUserId.openOr("0").toLong,
        poolId.toLong,
        Permission.Read) => unAuthorized
  }

  def dispatch: LiftRules.DispatchPF = {
    case Req("api2" :: "session" :: Nil, _, GetRequest) => allSessions
    case Req("api2" :: "session" :: Nil, _, PostRequest) => addSession
    case Req("api2" :: "session" :: Nil, _, DeleteRequest) => removeSession

    case Req("api2" :: "users" :: Nil, _, GetRequest) => allUsers
    case Req("api2" :: "users" :: Nil, _, PostRequest) => addUser
    case Req("api2" :: "users" :: id :: "tokens" :: Nil, _, GetRequest) => () => allTokens(id)
    case Req("api2" :: "users" :: id :: "tokens" :: Nil, _, PostRequest) => () => addToken(id)

    case Req("api2" :: "user" :: "messages" :: Nil, _, GetRequest)
      if S.param("timeout").isDefined => waitForMsgs
    case Req("api2" :: "user" :: "messages" :: Nil, _, GetRequest)
      if S.param("history").isDefined => allUserMsgs
    case Req("api2" :: "user" :: "messages" :: Nil, _, GetRequest) => getNewMsgs
    case Req("api2" :: "user" :: "messages" :: Nil, _, PostRequest) => () => addMsg

    case Req("api2" :: "tags" :: tag :: "messages" :: Nil, _, GetRequest)
            => () => allTagMsgs(tag)

    case Req("api2" :: "user" :: "followees" :: Nil, _, GetRequest) => allFollowees
    case Req("api2" :: "user" :: "followees" :: Nil, _, PostRequest) => addFollowee
    case Req("api2" :: "user" :: "followees" :: userId :: Nil, _, DeleteRequest)
            => removeFollow(Box(List(userId)))

    case Req("api2" :: "user" :: "followers" :: Nil, _, GetRequest) => allFollowers

    case Req("api2" :: "user" :: "tracks" :: Nil, _, GetRequest) => allTracking
    case Req("api2" :: "user" :: "tracks" :: Nil, _, PostRequest) => addTracking
    case Req("api2" :: "user" :: "tracks" :: trackId :: Nil, _, DeleteRequest) => ()
            => removeTracking(Box(List(trackId)))

    case Req("api2" :: "user" :: "actions" :: Nil, _, GetRequest) => allActions
    case Req("api2" :: "user" :: "actions" :: Nil, _, PostRequest) => addAction
    case Req("api2" :: "user" :: "actions" :: actionId :: Nil, _, PutRequest) => ()
            => changeAction(Box(List(actionId)))
    case Req("api2" :: "user" :: "actions" :: actionId :: Nil, _, DeleteRequest) => ()
            => removeAction(Box(List(actionId)))

    case Req("api2" :: "pools" :: Nil, _, GetRequest) => allPools
    case Req("api2" :: "pools" :: Nil, _, PostRequest) => () => addPool
    case Req("api2" :: "pools" :: poolId :: "users" :: Nil, _, PostRequest) => ()
            => addUserToPool(poolId)
    case Req("api2" :: "pools" :: poolId :: "messages" :: Nil, _, GetRequest)
      if S.param("timeout").isDefined => () => waitForPoolMsgs(poolId)
    case Req("api2" :: "pools" :: poolId :: "messages" :: Nil, _, GetRequest)
      if S.param("history").isDefined => () => histPoolMsgs(poolId)
    case Req("api2" :: "pools" :: poolId :: "messages" :: Nil, _, GetRequest) => ()
            => getPoolMsgs(poolId)

    case Req("api2" :: "conversations" :: conversationId :: Nil, _, GetRequest) => ()
            => getConversation(Box(List(conversationId)))
  }

  def allSessions(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser)
      yield {
        (200,Map(),Full(<session>{userToXml(user)}</session>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((404,Map(),Empty))

    r
  }

  def addSession(): LiftResponse = {
    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] = if (User.loggedIn_?) Empty else
    for(token <- S.param("token")) yield {
      val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] = for {
        auth <- AuthToken.find(By(AuthToken.uniqueId, token))
        user <- auth.user.obj
        val user_xml: Elem = <session>{userToXml(user)}</session>
      } yield {
        User.logUserIn(user)
        val myActor = buildActor(user.id)
        messageRestActor(Full(myActor))
        userRoles(AuthRole("integration-admin"))
        (200,Map(),Full(user_xml))
      }

      ret openOr (403,Map(),Empty)
    }

    r
  }

  def removeSession(): LiftResponse = {
    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if (User.loggedIn_?) {
        User.logUserOut()
        Full((200,Map(),Empty))
      } else Full((404,Map(),Empty))

    r
  }


  def allUsers(): LiftResponse = {
    val users: NodeSeq = for (user <- User.findAll) yield userToXml(user)
    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      Full(if (User.loggedIn_?) (200,Map(),Full(<users>{users}</users>)) else (403,Map(),Empty))

    r
  }

  def addUser(): LiftResponse = {
    val moduleName: String = "upw"

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] = {
      for{
        nickName <- S.param("nickname")
        passWord <- S.param("password")
      } yield {
        User.findByNickname(nickName) match {
          case user :: _ => (200,Map(),Full(userToXml(user)))
          case _ =>
            val user = User.createAndPopulate.nickname(nickName).saveMe
            val salt = randomString(10)
            val md5 = Helpers.md5(salt + passWord)
            UserAuth.create
                    .user(user)
                    .authType(moduleName)
                    .authKey(nickName)
                    .authData(salt+";"+md5)
                    .save
            (200,Map(),Full(userToXml(user)))
        }
      }
    }

    r
  }

  def allTokens(userId: String): LiftResponse = {
    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] = {
      for{
        user <- User.find(userId)
      } yield {
        val tokens: NodeSeq = user.authTokens.map(t => tokenToXml(t))
        (200,Map(),Full(<tokens>{tokens}</tokens>))
      }
    }

    r
  }

  def addToken(userId: String): LiftResponse = {
    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] = {
      for{
        user <- User.find(userId)
      } yield {
        val token: AuthToken = AuthToken.create
          .user(user)
          .description(S.param("description"))
          .saveMe
        (200,Map(),Full(tokenToXml(token)))
      }
    }

    r
  }

  def allUserMsgs(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
        val num = S.param("history").map(_.toInt) openOr 40;
        val lst = Mailbox.mostRecentMessagesFor(user.id, num))
      yield (200,Map(),Full(<messages>{lst.flatMap{ case (msg, reason, _) => msgToXml(msg) }}</messages>))
                
    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def getNewMsgs(): LiftResponse = {
    val future = new LAFuture[List[(Message, MailboxReason)]]()

    def waitForAnswer: Box[List[(Message, MailboxReason)]] =
      future.get(60L * 1000L)

    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (act <- messageRestActor.is ?~ S.?("base_rest_api_err_no_rest_actor");
           val ignore = act ! ListenFor(future, 0 seconds);
           answer <- waitForAnswer ?~ S.?("base_rest_api_err_no_answer"))
      yield {
        if(answer.isEmpty) (304,Map(),Empty)
        else (200,Map(),Full(<messages>{answer.flatMap{ case (msg, reason) => msgToXml(msg) }}</messages>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def waitForMsgs(): LiftResponse = {
    val future = new LAFuture[List[(Message, MailboxReason)]]()

    def waitForAnswer: Box[List[(Message, MailboxReason)]] =
      future.get(6L * 60L * 1000L)

    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (act <- messageRestActor.is ?~ "No REST actor";
        length <- S.param("timeout").map(_.toInt * 1000);
        val ignore = act ! ListenFor(future, TimeSpan(length));
        answer <- waitForAnswer ?~ "Didn't get an answer")
      yield {
        if(answer.isEmpty) (304,Map(),Empty)
        else (200,Map(),Full(<messages>{answer.flatMap{ case (msg, reason) => msgToXml(msg) }}</messages>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def allTagMsgs(tag: String): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           tagName <- Box(List(tag));
           tag <- Tag.find(By(Tag.name, tagName)))
      yield {
        val tag_xml = <tag><name>{tag.name}</name><messages>{tag.findMessages.map(msgToXml(_))}</messages></tag>
        (200,Map(),Full(tag_xml))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def addMsg(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser.map(_.id.is);
           msg <- S.param("message"))
      yield {
        val from: String = S.param("via") openOr "api2"
        val pool = for (poolName <- S.param("pool");
                        p <- AccessPool.findPool(poolName,
                        S.param("realm") openOr AccessPool.Native)
                        ) yield p.id.is
        val xml: Box[Elem] =
          S.param("metadata").flatMap(md =>
            tryo(XML.loadString(md)))

        Distributor !! Distributor.UserCreatedMessage(user, msg,
                                       Tag.split(S.param("tags")
                                                 openOr ""),
                                       millis,
                                       xml match {
                                         case Full(x) => xml
                                         case _ => Box({new Atom(S.param("metadata") openOr "")})
                                       },
                                       from,
                                       S.param("replyto").map(toLong),
                                       pool) match {
           case Full(m: Message) => (200,Map(),Full(msgToXml(m)))
           case other => (200,Map(),Empty)
        }                            
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }


  def allFollowees(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for(user <- User.currentUser)
      yield {
        val followees: NodeSeq =
          User.currentUser.map(_.following)
                          .map(_.map(userToXml(_)))
                          .openOr(<no_followees/>)
        (200,Map(),Full(<followees>{followees}</followees>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def addFollowee(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
        userName <- S.param("userId");
        other <- User.findFromWeb(userName))
      yield {
        user.follow(other)
        (200,Map(),Full(userToXml(other)))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }


  def removeFollow(userName: Box[String])(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
        userName <- userName;
        other <- User.findFromWeb(userName))
      yield {
        user.unfollow(other)
        (200,Map(),Empty)
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def allFollowers(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for(user <- User.currentUser)
      yield {
        val followees: NodeSeq =
          User.currentUser.map(_.following)
                          .map(_.map(userToXml(_)))
                          .openOr(<no_followees/>)
        (200,Map(),Full(<followees>{followees}</followees>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def allTracking(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
      yield {
        val track_lst = Tracking.findAll(By(Tracking.user, user)).flatMap(_.toXml)
        (200,Map(),Full(<tracks>{track_lst}</tracks>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def addTracking(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           toTrack <- S.param("track") if toTrack.trim.length > 0)
      yield
        (200,Map(),Full(<track>{Tracking.create.user(user).regex(toTrack).save}</track>))

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def removeTracking(trackId: Box[String]): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           id <- trackId;
           track <- Tracking.find(By(Tracking.id, id.toLong),
                                  By(Tracking.user, user)) ?~ S.?("base_rest_api_err_param_no_tracking"))
      yield {
        track.removed(true).save
        (200,Map(),Empty)
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def allActions(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
      yield (200,Map(),Full(<actions>{user.performingwithdisabled.flatMap(_.toXml)}</actions>))

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def addAction(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
           name <- S.param("name") ?~ S.?("base_rest_api_err_missing_param", "name");
           test <- S.param("test") ?~ S.?("base_rest_api_err_missing_param", "test");
           action <- S.param("action") ?~ S.?("base_rest_api_err_missing_param", "action");
           val a = Action.create.user(user).name(name);
           a2 <- a.setTest(test);
           a3 <- a.setAction(action))
       yield {
         (200,Map(),Full(a3.saveMe.toXml))
       }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def changeAction(actionId: Box[String]): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           action <- findAction(actionId);
           enabled <- S.param("enabled").map(toBoolean) ?~ S.?("base_rest_api_err_missing_param", "enable"))
      yield {
        action.disabled(!enabled).save
        (200,Map(),Full(action.toXml))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def removeAction(actionId: Box[String]): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           action <- findAction(actionId))
      yield {
        action.removed(true).save
        (200,Map(),Empty)
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def allPools(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in"))
      yield {
        val pools_lst = AccessPool.findAll(In(AccessPool.id,
                                              Privilege.pool,
                                              By(Privilege.user, user)),
                                           OrderBy(AccessPool.id, Descending),
                                           MaxRows(20)).flatMap(_.toXml)
        (200,Map(),Full(<pools>{pools_lst}</pools>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def addPool(): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           pool <- AccessPool.create.realm(AccessPool.Native).setName(S.param("poolName").openOr(""));
           privilegeSaved = Privilege.create.pool(pool.saveMe)
                                     .user(user)
                                     .permission(Permission.Admin)
                                     .save)
      yield {
        if (privilegeSaved) Distributor ! Distributor.AllowUserInPool(user.id.is, pool.id.is)
        (200,Map(),Full(pool.toXml))
    }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def addUserToPool(poolId: String): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (adminUser <- User.currentUser;
           realm <- (S.param("realm") or Full(AccessPool.Native));
           pool <- AccessPool.find(By(AccessPool.id, poolId.toLong),
                        By(AccessPool.realm, realm));
           userName <- S.param("userId");
           user <- User.findFromWeb(userName);
           permissionName <- (S.param("permission") or Full("Write"));
           permission <- Box(Permission.valueOf(permissionName)))
      yield
        if(Privilege.hasPermission(adminUser.id.is, pool.id.is, Permission.Admin)) {
          val result = try {
            Privilege.create.user(user).pool(pool).permission(permission).save
          } catch {
            case _: Exception => false
          }

          if (result) Distributor ! Distributor.AllowUserInPool(user.id.is, pool.id.is)
            (200,Map(),Full(userToXml(user)))
        } else (403,Map(),Empty) // "User has no permission to administer pool"

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def histPoolMsgs(poolId: String): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
        val poolNum = poolId.toInt;
        val num = S.param("history").map(_.toInt) openOr 40)
      yield {
        val boxed_lst: Box[List[Message]] =
        for(session <- compass.map(_.openSession()); user <- User.currentUser)
        yield {
          var tx:CompassTransaction = null
          var returnValue:List[Message] = Nil

          try {
            tx = session.beginTransaction()
            val queryBuilder: CompassQueryBuilder = session.queryBuilder()

            val tagQuery = queryBuilder.bool()

            for(tags <- S.param("filter_tags");
                tag <- tags.split(",")) {  
              tagQuery.addMust(queryBuilder.term("tags", tag.split(" ").mkString("_").toLowerCase()))
            }

            val non_tag_query = queryBuilder.bool()
              .addMust(queryBuilder.term("pool", poolNum))


            val query = if(S.param("filter_tags").isDefined)
              non_tag_query.addMust(tagQuery.toQuery()).toQuery()
            else
              non_tag_query.toQuery()

            val hitlist = query
              .addSort("when", CompassQuery.SortPropertyType.STRING, CompassQuery.SortDirection.REVERSE)
              .hits().detach(0, num)

            val resourceList = hitlist.getResources.toList.asInstanceOf[List[Resource]]

            val msgIds = resourceList.map(_.getId.toLong)
            returnValue = Message.findMessages(msgIds).values.toList
            tx.commit();
          } catch  {
            case ce: CompassException =>
              if (tx != null) tx.rollback();
          } finally {
            session.close();
          }
          returnValue
        }

        val lst: List[Message] = boxed_lst.openOr(List())

        (200,Map(),Full(<messages>{lst.flatMap(msgToXml(_))}</messages>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def getPoolMsgs(poolId: String): LiftResponse = {
    val future = new LAFuture[List[(Message, MailboxReason)]]()

    def waitForAnswer: Box[List[(Message, MailboxReason)]] =
      future.get(60L * 1000L)

    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           act <- poolRestActors.findOrCreate(poolId.toLong);
           val ignore = act ! ListenFor(future, 0 seconds);
           answer <- waitForAnswer)
      yield {
        if(answer.isEmpty) (304,Map(),Empty)
        else (200,Map(),Full(<messages>{answer.flatMap{ case (msg, reason) => msgToXml(msg) }}</messages>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def waitForPoolMsgs(poolId: String): LiftResponse = {
    val future = new LAFuture[List[(Message, MailboxReason)]]()

    def waitForAnswer: Box[List[(Message, MailboxReason)]] =
      future.get(6L * 60L * 1000L)

    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           act <- poolRestActors.findOrCreate(poolId.toLong);
           length <- S.param("timeout").map(_.toInt * 1000);
           val ignore = act ! ListenFor(future, TimeSpan(length));
           answer <- waitForAnswer)
      yield {
        if(answer.isEmpty) (304,Map(),Empty)
        else (200,Map(),Full(<messages>{answer.flatMap{ case (msg, reason) => msgToXml(msg) }}</messages>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def getConversation(conversationId: Box[String]): LiftResponse = {
    val ret: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      for (user <- User.currentUser;
           id <- conversationId.map(toLong))
      yield {
        val messages =
          Message.findAndPrime(By(Message.conversation, id),
                               OrderBy(Message.id, Ascending))

        if(messages.isEmpty)
          (404,Map(),Empty)
        else
          (200,Map(),Full(<conversation id={id.toString}>{messages.map(msgToXml(_))}</conversation>))
      }

    val r: Box[Tuple3[Int,Map[String,String],Box[Elem]]] =
      if(ret.isDefined) ret else Full((403,Map(),Empty))

    r
  }

  def unAuthorized(): LiftResponse = {
    Full((403,Map(),Empty))
  }

  private def findAction(actionId: Box[String]): Box[Action] =
    for (user <- User.currentUser ?~ S.?("base_rest_api_err_not_logged_in");
         id <- actionId ?~ S.?("base_rest_api_err_missing_param", "id");
         action <- Action.find(By(Action.user, user),
                             By(Action.id, id.toLong),
                             By(Action.removed, false))) yield action

  def createTag(in: NodeSeq) = <api>{in}</api>

  private def buildActor(userId: Long): RestActor = {
    val ret = new RestActor
    ret ! StartUp(userId)
    ret
  }

  private def buildPublicTimelineActor(matcher: Function1[Message,Boolean]): RestActor = {
    val ret = new RestActor(matcher)
    ret ! StartUpPublic
    ret
  }

  object messageRestActor extends SessionVar[Box[RestActor]](Empty) {
    override def onShutdown(session: LiftSession) = this.is.map(_ ! ByeBye)
  }

  object poolRestActors extends SessionVar[Map[Long,RestActor]](Map()) {
    override def onShutdown(session: LiftSession) = this.is.values.map(_ ! ByeByePublic)

    def poolMatcher(msg: Message): Boolean =
      msg.pool == 1

    def findOrCreate(pool: Long): Box[RestActor] = {
      Full(this.getOrElse(pool, {
        def partialMatcher(msg: Message) = { msg.pool == pool }
        val newActor: RestActor = buildPublicTimelineActor(partialMatcher _)
        this.update((oldMap) => oldMap+((pool, newActor)))
        newActor
      }))
    }
  }

  class RestActor(msgMatch: Function1[Message,Boolean]) extends LiftActor {
    private var userId: Box[Long] = Empty
    private var msgs: List[(Message, MailboxReason)] = Nil
    private var listener: Box[LAFuture[List[(Message, MailboxReason)]]] = Empty

    def this() = this((msgToTest: Message) => true)

    protected def messageHandler = {
      case StartUp(userId) =>
        this.userId = Full(userId)
        Distributor ! Distributor.Listen(userId, this)

      case StartUpPublic =>
        Distributor ! Distributor.PublicTimelineListeners(this)

      case ByeBye =>
        Distributor ! Distributor.Unlisten(userId.openOr(0), this)

      case ByeByePublic =>
        Distributor ! Distributor.PublicTimelineUnlisteners(this)

      case UserActor.MessageReceived(msg, reason) =>
        reason match {
          case r: RegularReason => {}
          case _ =>
            msg match {
              case _ if msgMatch(msg) =>
                msgs = (msg, reason) :: msgs
                listener.foreach {
                  who =>
                    who.satisfy(msgs)
                    listener = Empty
                    msgs = Nil
                }
            }
        }

      case Distributor.NewMessage(msg) =>
        msg match {
          case _ if msgMatch(msg) =>
            msgs = (msg, NoReason) :: msgs
            listener.foreach {
              who =>
                who.satisfy(msgs)
                listener = Empty
                msgs = Nil
            }
          
          case _ => // avoid the match error
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
  private case object StartUpPublic
  private case object ByeBye
  private case object ByeByePublic
  private case class ListenFor(who: LAFuture[List[(Message, MailboxReason)]],
                   howLong: TimeSpan)
  private case object ReleaseListener
}                                                        