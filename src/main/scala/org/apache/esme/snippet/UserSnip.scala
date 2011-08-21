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

package org.apache.esme.snippet

import org.apache.esme._
import model._
import org.apache.esme.actor._

import net.liftweb._
import http._
import js._
import js.jquery._
import http.jquery._
import JqJsCmds._
import JsCmds._ 
import SHtml._
import JE._
import util._
import common._
import Helpers._
import TimeHelpers.intToTimeSpanBuilder
import Helpers.TimeSpan

import net.liftweb.util.Helpers.pairToUnprefixed
import scala.xml.MetaData

import scala.xml.{NodeSeq, Text, Node}

object JsonPoster extends JsonHandler{
  def apply(in: Any): JsCmd = in match {
    case JsonCmd("post", _, map: Map[String, Any], _) =>
      DisplayMessage("messages", <b>Status updated</b>, 200, 200)
      for (msgObj <- map.get("msg");
           msg <- Box.asA[String](msgObj).map(_.trim) if msg.length > 0;
           tagObj <- map.get("tags");
           tags <- Box.asA[String](tagObj);
           user <- User.currentUser) {

        val replyTo: Box[Long] = map.get("reply-to").map(toLong)

        val pool = map.get("access_pool").map(toLong) match {
          case Some(x) if x > 0L => Some(x)
          case _ => None
        }
        



        
        Distributor ! 
        Distributor.UserCreatedMessage(user.id, msg, 
                                       Tag.split(tags),
                                       millis, 
                                       Empty,
                                       "web",
                                       replyTo,
                                       pool)
      }
      Noop

    case _ => Noop
  }
}

class UserSnip extends DispatchSnippet {

  def dispatch: DispatchIt = 
  Map("name" -> userName _,
      "userImage" -> userImage _,
      "postScript" -> postScript _,
      "followers" -> followers _,
      "following" -> following _,
      "loginForm" -> loginForm _,
      "loggedIn" -> loggedInFilter _,
      "accessPools" -> accessPools _,  
      "popular" -> popular _,
      "links" -> links _)

  def loggedInFilter(in: NodeSeq): NodeSeq = {
    val lookFor = if (User.loggedIn_?) "in" else "out"
    
    (<foo>{in}</foo> \ lookFor).toList.
    filter(_.prefix == "logged").
    map(_.child).headOption.getOrElse(NodeSeq.Empty)
  }

  def userFmt(u: User): Node = 
   <li> <img width="30px" src={(u.image_url)} id="avatar"/> <a href={"/user/"+urlEncode(u.nickname.is)}>{u.niceName}</a> {u.firstName} {u.lastName}</li>



  def calcUser: Box[User] =
  S.attr("userId").flatMap(s => User.find(toLong(s))) or User.currentUser

  def followers(in: NodeSeq): NodeSeq = 
    {
      calcUser.toList.flatMap(_.followers.map(userFmt))
    }

  def following(in: NodeSeq): NodeSeq =
    {
      calcUser.toList.flatMap(_.following.map(userFmt))
    }

  def loginForm(in: NodeSeq): NodeSeq =
  if (User.loggedIn_?) NodeSeq.Empty
  else UserAuth.loginPresentation.map(l => <div>{l}</div>)
  

  def userName(in: NodeSeq) = {
    if (User.currentUser.map(_.needsChange_?) openOr false)
    S.redirectTo("/user_mgt/edit")
    
    Text(User.currentUser.map(_.wholeName) openOr "")
  }
  

   // Image of user as part of an img tag
  
  def image: MetaData = ("src" -> (User.currentUser.map(_.image_url)openOr "/images/avatar.jpg"))
  
  // Href used to display details about a user
  def userDetailshref: MetaData = ("href" -> ("/user/" + (User.currentUser.map(_.nickname.is)openOr "default")))

  
  def userImage(in: NodeSeq) = {
    if (User.currentUser.map(_.needsChange_?) openOr false)
    S.redirectTo("/user_mgt/edit")
    
    Text(User.currentUser.map(_.image_url) openOr "")
  }
  /*
  def accessPools(in: NodeSeq): NodeSeq = {
    for(user <- User.currentUser.toSeq;
        p    <- Privilege.findWritablePools(user.id))
        // slow?
      yield <option value={p}>
              {AccessPool.find(p).get.getName}
            </option>
  }
  */

  def accessPools(in: NodeSeq): NodeSeq = {
    (for(user <- User.currentUser.toSeq;
        p    <- Privilege.findWritablePools(user.id))
        // slow?
      yield p)
        .map( p => {val pool = AccessPool.find(p); (pool.get.id.is.toString, pool.get.getName)} )
        .toList
        .sortWith( ( first: (String, String), second: (String, String)) => first._2.toUpperCase < second._2.toUpperCase)
        .map(p => <option value={p._1}>{p._2}</option>)
  }
  
  def postScript(in: NodeSeq): NodeSeq =
  <xml:group>
    {Script(JsonPoster.jsCmd)}
    {Script(Function("post_msg", List(),
                     JsonPoster.call("post",
                                      JsObj("msg" -> ValById("vMsg"),
                                            "tags" -> ValById("vTag"),
                                            "access_pool" -> ValById("vPool"),
                                            "reply-to" -> JsVar("currentConvNumber"))) &
                     SetValById("vMsg", "") &
                     Run("clearReplyTo()")
        ))
    }
  </xml:group>
  
  def popular(in: NodeSeq): NodeSeq = 
    PopStatsActor !? PopStatsActor.TopStats(ResendStat, 5, 1 week) match {
      case l: List[Tuple2[Long,Int]] =>
        val msgMap = Message.findMessages(l.map(_._1))
        val messages = 
          for ((id, freq) <- l;
               msg <- msgMap.get(id))
          yield (msg, freq)
        messages match {
          case Nil => NodeSeq.Empty
          case xs => bind("disp", in,
                          "item" ->
                          (lst => xs.flatMap{case (m,freq) => bind(
                            "item", lst,
                            "freq" -> freq,
                            "author" -> m.author.obj.map(_.nickname.is).openOr(""),
                            "text" -> m.digestedXHTML,
                            "date" -> new java.util.Date(m.when.toLong).toString)}))
        }
      case _ => NodeSeq.Empty
    }
    
    //, u.url.is
    
  def links(in: NodeSeq): NodeSeq = 
    PopStatsActor !? PopStatsActor.TopStats(LinkClickedStat, 5, 1 week) match {
      case Nil => NodeSeq.Empty
      case xs: List[Tuple2[Long,Int]] =>
        bind("disp", in,
             "item" ->
             (lst => xs.flatMap {
               case (linkId, freq) =>
                 UrlStore.find(linkId).map { u => bind(
                   "item", lst,
                   "freq" -> freq,
                   "url" ->  <a href={u.url.is} target="_blank">{u.url.is}</a>
                   )
                 }.getOrElse(NodeSeq.Empty)
               }
             )
        )
      case _ => NodeSeq.Empty
    }
}
