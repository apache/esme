package us.esme.view

/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by apwant to grabplicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */


import net.liftweb._
import http._
import js._
import JE._
import JsCmds._
import SHtml._
import util._
import Helpers._
import sitemap._
import mapper._
import Loc._

import us.esme._
import model._
import lib._

import scala.xml.{NodeSeq, Text, Elem}

import java.util.logging._

object UserView extends LiftView /* with MsgFormat */ {
  val logger: Logger = Logger.getLogger("us.esme.view")
  logger.setLevel(Level.INFO)
  def loggedIn_? = User.loggedIn_?
  
  val ifIsLoggedIn = If(loggedIn_? _, "You must be logged in")

  val menuItems =
  Menu(Loc("userView", List("user_view", "index"), "View User", Hidden)) ::
  Nil

  implicit def fToO(in: () => NodeSeq): Box[() => NodeSeq] = Full(in)
  implicit def fToO2(in: Elem): Box[NodeSeq] = Full(in)

  val dispatch = 
  Map("index" -> index _,
      "tag" -> displayTag _,
      "search" -> search _,
      "all" -> allUsers _,
      "conversation" -> conversation _)

  def conversation(): Box[NodeSeq] =   {
    val cid = S.param("cid").map(toLong).openOr(-1L)

    val msgs = Message.findAndPrime(By(Message.conversation, cid),
                                    OrderBy(Message.id, Ascending))

    val replyMap = msgs.foldLeft[Map[Long, List[Message]]](Map.empty){
      case (map, msg) => msg.replyTo.can match {
          case Full(rt) => map + (rt -> (msg :: map.getOrElse(rt, Nil)))
          case _ => map
        }
    }
    
    val start = msgs.firstOption.toList

    def show(what: List[Message]): NodeSeq = {
      what match {
        case Nil => Text("")
        case _ =>
          <ul>
            {
              what.zipWithIndex.map{
                case (m, idx) =>
                  <span id={"m_"+idx}>
                    {Script(OnLoad(JsFunc("displayMessages", JsArray(m.asJs), "m_"+idx).cmd))
                    }
                  </span>
              }
            }
          </ul>
      }
    }
    
    <lift:surround with="default" at="content">
      <div>
        <fieldset>
          <legend>Conversation</legend>
          {
            show(start)
          }
        </fieldset>
      </div>

    </lift:surround>
  }

  def index(): Box[NodeSeq] =
  (<lift:surround with="default" at="content">
      {
        val ui = for (uid <- S.param("uid");
                      user <- User.findFromWeb(uid)) yield {
          def updateFollow: JsCmd = SetHtml("following", followOrNot)

          def followOrNot: NodeSeq = {
            User.currentUser match {
              case Full(u) if u != user =>
                if (u.following_?(user))
                ajaxButton("Unfollow", () => {u.unfollow(user); updateFollow})
                else ajaxButton("Follow", () => {u.follow(user); updateFollow})

              case _ => <xml:group />
            }
          }

          <div>
            <div id="tabs" class="b-view tab1">
              <dl class="messages">
                <dt class="caption">{user.niceName}'s Timeline 
                  <span id="following">{followOrNot}</span></dt>
                <dd>
                  {
                    val lst: List[Message] =
                    Mailbox.mostRecentMessagesFor(user.id, 50).map(_._1)
                    //  user.latestMessages(50) openOr Nil
                    lst.zipWithIndex.map{
                      case (m, idx) =>
                        <span id={"m2_"+idx}>
                          {
                            Script(OnLoad(JsFunc("displayMessages", JsArray(m.asJs), "m2_"+idx).cmd))
                          }
                        </span>
                    }
                  }
                </dd>
              </dl>
		  
              <dl class="tagclouds">
                <dt class="caption">{user.niceName}'s Messages</dt>
               
                <dd class="b-clouds">
                  {
                    val lst: List[Message] =
                    Message.findAll(By(Message.author, user), MaxRows(50), OrderBy(Message.id, Descending))
                    lst.zipWithIndex.map{
                      case (m, idx) =>
                        <span id={"m3_"+idx}>
                          {
                            Script(OnLoad(JsFunc("displayMessages", JsArray(m.asJs), "m3_"+idx).cmd))
                          }
                        </span>
                    }
                  }
                </dd>
              </dl>
              <dl class="contacts">
                <dt class="caption">
                  Contacts
                </dt>

                <dd class="b-contacts" style="height: 240px; overflow: auto">
                  Following:
                  <lift:UserSnip.following userId={user.id.toString}/>
                </dd>
                <dd class="b-contacts" style="height: 240px; overflow: auto">
                  Followers:
                  <lift:UserSnip.followers userId={user.id.toString}/>
                </dd>
              </dl>
            </div>
          </div>
        }
        ui openOr (<span>User Not Found</span>)
      }
   </lift:surround>)
  
  def search(): Box[NodeSeq] =
  for (user <- User.currentUser;
       term <- S.param("term")) yield
  <lift:surround with="default" at="content">
    <div>
      <fieldset>
        <legend>Search: {term}</legend>
        <ul>
          {
            val lst: List[Message] = Message.search(term, user.following, 50)
            lst.zipWithIndex.map{
              case (m, idx) =>
                <span id={"m4_"+idx}>
                  {
                    Script(OnLoad(JsFunc("displayMessages", JsArray(m.asJs), "m4_"+idx).cmd))
                  }
                </span>
            }
          }
        </ul>
      </fieldset>
    </div>
  </lift:surround>
  
  def displayTag(): Box[NodeSeq] =
  (<lift:surround with="default" at="content">
      {
        val ui = for (tagText <- S.param("tag").map(_.trim);
                      tag <- Tag.find(By(Tag.name, tagText))) yield {
          val lst = tag.findMessages()
          
          (<div>
              <fieldset>
                <legend>{tag.name}</legend>
                <ul>
                  {
                    lst.zipWithIndex.map{
                      case (m, idx) =>
                        <span id={"m5_"+idx}>
                          {
                            Script(OnLoad(JsFunc("displayMessages", JsArray(m.asJs), "m5_"+idx).cmd))
                          }
                        </span>
                    }
                  }
                </ul>
              </fieldset>
           </div>)
        }
        ui openOr (<span>Tag Not Found</span>)
      }
   </lift:surround>)
  
  def allUsers(): Box[NodeSeq] = 
  <lift:surround with="default" at="content">
    <fieldset>
      <legend>Users</legend>
      <ul>
        {
          User.findAll(OrderBy(User.nickname, Ascending)).
          map(u => <li><a href={"/user/"+urlEncode(u.nickname.is)}>{u.niceName}</a> {u.firstName} {u.lastName}</li>)
        }
      </ul>
    </fieldset>
  </lift:surround>

}
