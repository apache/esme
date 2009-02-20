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
 * RestAPI.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.apache.esme.api

import net.liftweb._
import http._
import auth._
import js._
import JE._
import rest._
import util._
import mapper._
import Helpers._

import org.apache.esme._
import model._
import actor._

import scala.xml._
import scala.actors.Actor
import Actor._

import scala.collection.mutable.ListBuffer
import java.util.logging._

object TwitterAPI {
  val ApiPath = Props.get("twitter.prefix", "twitter") split "/" toList
  
  def twitterAuth = HttpBasicAuthentication("esme") {
    case (user: String, password: String, _) =>
      !(for(auth <- AuthToken.find(By(AuthToken.uniqueId, password));
           u <- auth.user.obj
           if u.nickname == user || u.id == user) yield {
        userRoles(AuthRole("user"))
      }).isEmpty
  }

}

abstract class TwitterAPI {
  import TwitterAPI.ApiPath
  
  val tf = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy", java.util.Locale.US)
  val logger: Logger = Logger.getLogger("org.apache.esme.api")
  val method: String
  // TODO: twitter struct could be stronger typed
  type TwitterResponse = Either[(String,List[Any]),Map[String,Any]]
  
  def dispatch: LiftRules.DispatchPF
  protected def dispatchMethod: PartialFunction[Req, () => Box[TwitterResponse]] = {
    case Req(l: List[String], this.method, GetRequest) if l == ApiPath ::: "statuses" :: "public_timeline" :: Nil => publicTimeline
    case Req(l: List[String], this.method, GetRequest) if l == ApiPath ::: "statuses" :: "replies" :: Nil => replies
    case Req(l: List[String], this.method, GetRequest) if l == ApiPath ::: "direct_messages" :: Nil => directMessages
    case Req(l: List[String], this.method, GetRequest) if l == ApiPath ::: "statuses" :: "friends_timeline" :: Nil => friendsTimeline
    case Req(l: List[String], this.method, GetRequest) if l == ApiPath ::: "statuses" :: "user_timeline" :: Nil => userTimeline
    // case Req(ApiPath :: "statuses" :: "show" :: Nil, this.method, GetRequest) => showStatus
    case Req(l: List[String], this.method, PostRequest) if l == ApiPath ::: "statuses" :: "update" :: Nil => update

    // case Req(ApiPath :: "statuses" :: "friends" :: Nil, this.method, GetRequest) => friends
    // case Req(ApiPath :: "statuses" :: "followers" :: Nil, this.method, GetRequest) => followers
    // case Req(ApiPath :: "users" :: "show" :: Nil, this.method, GetRequest) => showUser

    // case Req(ApiPath :: "friendships" :: "create" :: Nil, this.method, GetRequest) => createFriendship(S.param("user"))
    // case Req(ApiPath :: "friendships" :: "destroy" :: Nil, this.method, GetRequest) => destroyFriendship(S.param("user"))
    // case Req(ApiPath :: "friendships" :: "exists" :: Nil, this.method, GetRequest) => existsFriendship

    case Req(l: List[String], this.method, GetRequest) if l == ApiPath ::: "account" :: "verify_credentials" :: Nil => verifyCredentials
    // case Req(ApiPath :: "account" :: "end_session" :: Nil, this.method, GetRequest) => endSession
    // case Req(ApiPath :: "account" :: "rate_limit_status" :: Nil, this.method, GetRequest) => rateLimitStatus
    // case Req(ApiPath :: "update_profile" :: Nil, this.method, GetRequest) => updateProfile

  }

  def userAttributes(user: User) = {
    Map("id" -> user.id,
    "name" -> (user.firstName + " " + user.lastName),
    "screen_name" -> user.nickname,
    "location" -> user.timezone,
    "profile_image_url" -> user.imageUrl,
    "followers_count" -> user.followers.size,
    "description" -> "N/A",
    "url" -> "",
    "protected" -> false
    )
  }
  
  def msgAttributes(msg: Message) = {
    Map("created_at" -> tf.format(new java.util.Date(msg.when.is)),
    "id" -> msg.id.is,
    "text" -> msg.getText.trim,
    "source" -> msg.source,
    "truncated" -> false,
    "favorited" -> false,
    "in_reply_to_status_id" -> None,
    "in_reply_to_user_id" -> None,
    "in_reply_to_screen_name" -> None
    )
  }
  
  def userData(user: User) = {
    val lastMsg = Message.findAll(By(Message.author, user),
                                  OrderBy(Message.id, Descending),
                                  MaxRows(1))
    userAttributes(user) +
      (("status", lastMsg.map(msgAttributes _).firstOption.getOrElse("")))
  }
  
  def msgData(msg: Message) = {
    val msgUser = User.find(msg.author).get
    msgAttributes(msg) +
      (("user", userAttributes(msgUser)))
  }
  
  def verifyCredentials(): Box[TwitterResponse] = {
    calcUser map { user => Right(Map("user" -> userData(user))) }
  }

  def friendsTimeline(): Box[TwitterResponse] = {
    calcUser map { user => 
      val statusList =
        for ((msg, why) <- Mailbox.mostRecentMessagesFor(user.id, 20))
          yield { msgData(msg) }
      Right(Map("statuses" -> ("status", statusList) ))
    }
  }
  
  def userTimeline(): Box[TwitterResponse] = {
    calcUser map { user => 
      val statusList = 
        Message.findAll(By(Message.author, user),
                        MaxRows(20),
                        OrderBy(Message.id, Descending)).
          map(msgData _)
      Right(Map("statuses" -> ("status", statusList) ))
    }
  }
  
  def replies(): Box[TwitterResponse] = {
    userTimeline
  }

  def directMessages(): Box[TwitterResponse] = {
    Full(Left("direct_messages" -> Nil))
  }

  def publicTimeline(): Box[TwitterResponse] = {
    val statusList =
      Message.findAll(OrderBy(Message.id, Descending),
                      MaxRows(20)).
        map(msgData _)
    Full(Right(Map("statuses" -> ("status", statusList) )))
  }
  
  def update(): Box[TwitterResponse] = {
    for (req <- S.request;
         user <- calcUser ?~ "User not found";
         text <- req.param("status") ?~ "Message not included";
         msg <- Message.create.author(user.id.is).when(millis).
                               source(req.param("source") openOr "twitterapi").
                               setTextAndTags(text, Nil, None))
    yield {
      Distributor ! Distributor.AddMessageToMailbox(user.id.is, msg.saveMe, NoReason)
      Right(Map("status" -> msgData(msg)))
    }
  }

  private def calcUser(): Box[User] = 
    LiftRules.authentication match {
      case basicAuth: HttpBasicAuthentication =>
        for (req <- S.request;
             cred <- basicAuth.credentials(req);
             user <- User.findFromWeb(cred._1))
        yield user
    }

}

object TwitterXmlAPI extends TwitterAPI with XMLApiHelper {
  
  override val method = "xml"
  
  def toXml(o:Any): NodeSeq = { o match {
    case m: Map[Any,Any] =>  NodeSeq.fromSeq(
      m.foldRight(Nil: List[Node]) { (e: (_, _), ns: List[Node]) =>
        Elem(null, e._1.toString, Null, TopScope, toXml(e._2): _*) :: ns } 
    )
    case (label: String, list: List[Any]) => NodeSeq.fromSeq(
      list.map { e =>
        Elem(null, label.toString, Null, TopScope, toXml(e): _*) }
    )
    case None => Text("")
    case a: Any => Text(a.toString)}
  }

  override def dispatch: LiftRules.DispatchPF = {
    // modify the returned function to one which converts the result to XML
    dispatchMethod.andThen(x =>
      {() => Full(nodeSeqToResponse(toXml(Either.merge(x().get)))) }
    )
  }

  def createTag(in: NodeSeq) = in.first match {
    case e: Elem => e
    case _ => <error/>
  }
}
  
object TwitterJsonAPI extends TwitterAPI {

  override val method = "json"

  override def dispatch: LiftRules.DispatchPF = {
    // modify the returned function to one which converts the result to JSON
    dispatchMethod.andThen(x =>
      {() => Full(JsonResponse(jsonAttributes(Either.merge(x().get)))) }
    )
  }
  
  def jsonAttributes(o: Any): JsExp = { o match {
    case m: Map[String, Any] => toJson(m.values.next)
    case o => toJson(o)}
  }

  def toJson(o:Any): JsExp= { o match {
    case (label: String, list: List[Any]) =>
      new JE.JsArray(list map{ toJson })
    case m: Map[Any,Any] =>
      JE.JsObj(m.map{ e => (e._1.toString, toJson(e._2)) } toSeq : _* )
    case None => JE.JsNull
    case b: Boolean => b
    case i: Int => i
    case l: Long => l
    case a: Any => a.toString
    }
  }
  
}

