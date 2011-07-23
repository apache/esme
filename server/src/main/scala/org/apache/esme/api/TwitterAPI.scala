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

package org.apache.esme.api

import net.liftweb._
import http._
import actor._
import auth._
import js._
import JE._
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

// operator to match on last elements of a List
object ::> {def unapply[A] (l: List[A]) = l match {
    case Nil => None
    case _ => Some( (l.init, l.last) )
  }
}

object TwitterAPI {
  val ApiPath = Props.get("twitter.prefix", "twitter").split("/").toList.filter(_.length > 0)
  
  val ByMe = By(Mailbox.resent, true)
  val ToMe = NotBy(Mailbox.resentBy, Empty)
  
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
  import TwitterAPI._
  
  val tf = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy", java.util.Locale.US)
  val logger: Logger = Logger("org.apache.esme.twitterapi")
  val method: String
  // TODO: twitter struct could be stronger typed- if recursive types are enabled
  type TwitterResponse = Either[(String,List[Any]),Map[String,Any]]
  
  def dispatch: LiftRules.DispatchPF
  protected def dispatchMethod: PartialFunction[Req, () => Box[TwitterResponse]] = {
    case Req(ApiPath ::> "statuses" ::> "public_timeline", this.method, GetRequest) => publicTimeline
    case Req(ApiPath ::> "statuses" ::> "replies", this.method, GetRequest) => replies
    case Req(ApiPath ::> "direct_messages", this.method, GetRequest) => directMessages
    case Req(ApiPath ::> "statuses" ::> "friends_timeline", this.method, GetRequest) => friendsTimeline
    case Req(ApiPath ::> "statuses" ::> "user_timeline", this.method, GetRequest) => userTimeline
    case Req(ApiPath ::> "statuses" ::> "user_timeline" ::> last, this.method, GetRequest) => () => userTimeline(last)
    case Req(ApiPath ::> "statuses" ::> "home_timeline", this.method, GetRequest) => friendsTimeline
    case Req(ApiPath ::> "statuses" ::> "show" ::> last, this.method, GetRequest) => () => showStatus(last)
    case Req(ApiPath ::> "statuses" ::> "update", this.method, PostRequest) => update

    case Req(ApiPath ::> "statuses" ::> "friends", this.method, GetRequest) => friends
    case Req(ApiPath ::> "statuses" ::> "friends" ::> last, this.method, GetRequest) => () => friends(last)
    case Req(ApiPath ::> "statuses" ::> "followers", this.method, GetRequest) => followers
    case Req(ApiPath ::> "statuses" ::> "followers" ::> last, this.method, GetRequest) => () => followers(last)
    case Req(ApiPath ::> "users" ::> "show" ::> last, this.method, GetRequest) => () => showUser(last)

    case Req(ApiPath ::> "statuses" ::> "retweeted_by_me", this.method, GetRequest) => () => timeline(ByMe)
    case Req(ApiPath ::> "statuses" ::> "retweeted_to_me", this.method, GetRequest) => () => timeline(ToMe)

    case Req(ApiPath ::> "friendships" ::> "create" ::> last, this.method, PostRequest) => () => createFriendship(last)
    case Req(ApiPath ::> "friendships" ::> "create", this.method, PostRequest)
      if S.param("user_id").isDefined => createFriendship
    case Req(ApiPath ::> "friendships" ::> "destroy" ::> last, this.method, PostRequest) => () => destroyFriendship(last)
    case Req(ApiPath ::> "friendships" ::> "destroy", this.method, PostRequest)
      if S.param("user_id").isDefined => destroyFriendship
    case Req(ApiPath ::> "friendships" ::> "exists", this.method, GetRequest) => existsFriendship

    case Req(ApiPath ::> "account" ::> "verify_credentials", this.method, GetRequest) => verifyCredentials
    case Req(ApiPath ::> "account" ::> "end_session", this.method, PostRequest) => endSession
    // case Req(ApiPath ::> "update_profile", this.method, GetRequest) => updateProfile

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
    "text" -> msg.body.trim,
    "source" -> msg.source,
    "truncated" -> false,
    "favorited" -> false,
    "in_reply_to_status_id" ->
      Message.find(msg.conversation).
        map(_.id.is).getOrElse(None),
    "in_reply_to_user_id" ->
      Message.find(msg.conversation).
        map(_.author.is).getOrElse(None),
    "in_reply_to_screen_name" ->
      Message.find(msg.conversation).
        map(msg => 
            User.find(msg.author).get.nickname
        ).getOrElse(None)
    )
  }
  
  def extendedAttributes(user: User) = {
    Map(
      "profile_background_color" -> None,
      "profile_text_color" -> None,
      "profile_link_color" -> None,
      "profile_sidebar_fill_color" -> None,
      "profile_sidebar_border_color" -> None,
      "friends_count" -> user.following.size,
      "created_at" -> None,
      "favourites_count" -> 0,
      "utc_offset" -> 0,
      "time_zone" -> user.timezone,
      "profile_background_image_url" -> None,
      "profile_background_tile" -> false,
      "following" -> calcUser.map(_.following_?(user)).getOrElse(false),
      "notifications" -> false,
      "statuses_count " -> Message.count(By(Message.author, user))
    )
  }
  
  
  def userData(user: User) = {
    val lastMsg = Message.findAll(By(Message.author, user),
                                  By(Message.pool, Empty),
                                  OrderBy(Message.id, Descending),
                                  MaxRows(1))
    userAttributes(user) +
      (("status", lastMsg.map(msgAttributes _).headOption.getOrElse("")))
  }
  
  def extendedUserData(user: User) =
    userData(user) ++ extendedAttributes(user)
  
  def msgData(msg: Message) = {
    val msgUser = User.find(msg.author).get
    msgAttributes(msg) +
      (("user", userAttributes(msgUser)))
  }
  
  def verifyCredentials(): Box[TwitterResponse] = {
    calcUser map { user => Right(Map("user" -> userData(user))) }
  }

  def friendsTimeline(): Box[TwitterResponse] = {
    timeline(Ignore[Mailbox])
  }
  
  def userTimeline(user: User): TwitterResponse = {
    val queryParams = List(
      By(Message.author, user),
      By(Message.pool, Empty)) ++
      getCommonParams(Message.id)
    val statusList = Message.findAll(queryParams: _*).map(msgData _)
    Right(Map("statuses" -> ("status", statusList) ))
  }
  
  def userTimeline(userName: String): Box[TwitterResponse] = {
    User.findFromWeb(userName).map(userTimeline) ?~ S.?("base_twitter_api_err_user_not_found")
  }
  
  def userTimeline(): Box[TwitterResponse] = {
    calcUser map (userTimeline)
  }
  
  def replies(user: User): TwitterResponse = {
    val queryParams = List(
      In(Message.replyTo, Message.id, By(Message.author, user))) ++
      getCommonParams(Message.id)
    val statusList = Message.findAll(queryParams: _*).map(msgData _)
    Right(Map("statuses" -> ("status", statusList) ))
  }
  
  def replies(): Box[TwitterResponse] = {
    calcUser map (replies)
  }

  def directMessages(): Box[TwitterResponse] = {
    Full(Right(Map("direct-messages" -> ("direct_message", Nil))))
  }

  def publicTimeline(): Box[TwitterResponse] = {
    val queryParams = List[QueryParam[Message]](
      OrderBy(Message.id, Descending),
      MaxRows(getCount),
      By(Message.pool, Empty)) ++ getStart[Message]
    val statusList =
      Message.findAll(queryParams: _*).
        map(msgData _)
    Full(Right(Map("statuses" -> ("status", statusList) )))
  }
  
  def update(): Box[TwitterResponse] = {
    for (req <- S.request;
         user <- calcUser ?~ S.?("base_twitter_api_err_user_not_found");
         text <- req.param("status") ?~ S.?("base_twitter_api_err_user_not_included");
         msg <- Message.create.author(user.id.is).when(millis).
                               source(req.param("source") openOr "twitterapi").
                               setTextAndTags(text, Nil, None))
    yield {
      Distributor ! Distributor.UserCreatedMessage(user.id.is, text, Nil,
                                                   millis,
                                                   None,
                                                   msg.source,
                                                   None,
                                                   None)
      Right(Map("status" -> msgData(msg)))
    }
  }

  def friends(user: User): TwitterResponse = {
    Right(Map("users" -> ("user", user.following().map(userData)) ))
  }
  
  def friends(userName: String): Box[TwitterResponse] = {
    User.findFromWeb(userName).map(friends) ?~ S.?("base_twitter_api_err_user_not_found")
  }
  
  def friends(): Box[TwitterResponse] = {
    calcUser map(friends)
  }
  
  def followers(user: User): TwitterResponse = {
    Right(Map("users" -> ("user", user.followers().map(userData)) ))
  }
  
  def followers(userName: String): Box[TwitterResponse] = {
    User.findFromWeb(userName).map(followers) ?~ S.?("base_twitter_api_err_user_not_found")
  }
  
  def followers(): Box[TwitterResponse] = {
    calcUser map(followers)
  }
  
  def showUser(name: String): Box[TwitterResponse] = {
    for (user <- User.findFromWeb(name) ?~ S.?("base_twitter_api_err_user_not_found"))
    yield Right(Map("user" -> extendedUserData(user)))
  }

  def showStatus(statusId: String): Box[TwitterResponse] = {
    for (status <- Message.find(statusId) ?~ S.?("base_twitter_api_err_message_not_found"))
    yield Right(Map("status" -> msgData(status)))
  }

  def createFriendship(other: String): Box[TwitterResponse] = {
    for (user <- calcUser;
         other <- User.findFromWeb(other) ?~ S.?("base_twitter_api_err_user_not_found"))
    yield {
      if (user follow other)
        Right(Map("user" -> userData(other)))
      else
        Right(Map("hash" -> Map("error" -> S.?("base_twitter_api_err_user_not_follow"))))
    }
  }
  
  def createFriendship(): Box[TwitterResponse] = {
    for (user_id <- S.param("user_id");
         response <- createFriendship(user_id))
    yield response
  }
  
  def destroyFriendship(other: String): Box[TwitterResponse] = {
    for (user <- calcUser;
         other <- User.findFromWeb(other) ?~ S.?("base_twitter_api_err_user_not_found"))
    yield {
      if (user unfollow other)
        Right(Map("user" -> userData(other)))
      else
        Right(Map("hash" -> Map("error" -> S.?("base_twitter_api_err_user_not_unfollow"))))
    }
  }

  def destroyFriendship(): Box[TwitterResponse] = {
    for (user_id <- S.param("user_id");
         response <- destroyFriendship(user_id))
    yield response
  }
  
  def existsFriendship(): Box[TwitterResponse] = {
    for (req <- S.request;
         param_a <- req.param("user_a") ?~ S.?("base_twitter_api_err_user_a_not_specified");
         param_b <- req.param("user_b") ?~ S.?("base_twitter_api_err_user_b_not_specified");
         user_a <- User.findFromWeb(param_a) ?~ S.?("base_twitter_api_err_user_a_not_found");
         user_b <- User.findFromWeb(param_b) ?~ S.?("base_twitter_api_err_user_b_not_found"))
    yield Right(Map("friends" -> user_a.following_?(user_b)))
  }
  
  def endSession(): Box[TwitterResponse] = {
    calcUser map { _ =>
      User.logUserOut
      Right(Map("hash" -> Map("error" -> S.?("base_twitter_api_err_user_logged_out"))))
    }
  }
  
  def timeline(retweeted: QueryParam[Mailbox]): Box[TwitterResponse] =
    calcUser.map { user =>
      val queryParams = List[QueryParam[Mailbox]](
        retweeted,
        By(Mailbox.user, user)) ++
        getCommonParams(Mailbox.message)
        
      val msgIds = 
        Mailbox.findMap(queryParams: _*) (m => Full(m.message.is))
      val msgMap = Message.findMessages(msgIds)
      val statusList = msgIds.flatMap(msgMap.get).
          map(msgData _)
      Right(Map("statuses" -> ("status", statusList) ))
    }

  private def getCount() = S.param("count").map(_.toInt) openOr 20

  private def getSinceId[T <: Mapper[T]](field: MappedLong[T]) =
    S.param("since_id").map { since =>
      By_>(field, since.toLong)
    }

  private def getMaxId[T <: Mapper[T]](field: MappedLong[T]) =
    S.param("max_id").map { max =>
      By_<(field, max.toLong + 1L)
    }
  
  private def getStart[T <: Mapper[T]]() =
    S.param("page").map { page =>
      StartAt[T]((page.toInt - 1) * getCount)
    }
  
  private def getCommonParams[T <: Mapper[T]](field: MappedLong[T]): List[QueryParam[T]] = {
    List(MaxRows[T](getCount),
    OrderBy(field, Descending)) ++
    getStart[T] ++
    getSinceId(field) ++
    getMaxId(field)
  }
    
  private def calcUser(): Box[User] = {
    val userBox =
      LiftRules.authentication match {
        case basicAuth: HttpBasicAuthentication =>
          for (req <- S.request;
               cred <- basicAuth.credentials(req);
               user <- User.findFromWeb(cred._1))
          yield user
      }
    userBox ?~ S.?("base_twitter_api_err_auth_failed")
  }
  
  protected def unbox(x: () => Box[TwitterResponse]) = {
    ( x() match {
        case Full(res) => res
        case Empty => 
          Right(Map("response" -> Nil))
        case failMsg: Failure => 
          Right(Map("hash" -> Map("error" -> failMsg.messageChain)))
      }
    ).merge
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
      {() => Full(listElemToResponse(toXml(unbox(x)))) }
    )
  }

  def createTag(in: NodeSeq) = in.head match {
    case e: Elem => e
    case _ => <error/>
  }
}
  
object TwitterJsonAPI extends TwitterAPI {

  override val method = "json"

  override def dispatch: LiftRules.DispatchPF = {
    // modify the returned function to one which converts the result to JSON
    dispatchMethod.andThen(x =>
      {() => Full(JsonResponse(jsonAttributes(unbox(x)))) }
    )
  }
  
  def jsonAttributes(o: Any): JsExp = { o match {
    case m: Map[String, Any] => toJson(m.values.iterator.next)
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

