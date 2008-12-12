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

object UserView extends LiftView with MsgFormat {
  val logger: Logger = Logger.getLogger("us.esme.view")
  logger.setLevel(Level.INFO)
  def loggedIn_? = User.loggedIn_?
  
  val ifIsLoggedIn = If(loggedIn_? _, "You must be logged in")

  val menuItems =
  Menu(Loc("userView", List("user_view", "index"), "View User", Hidden)) ::
  Nil

  implicit def fToO(in: () => NodeSeq): Can[() => NodeSeq] = Full(in)
  implicit def fToO2(in: Elem): Can[NodeSeq] = Full(in)

  val dispatch = 
  Map("index" -> index _,
      "tag" -> displayTag _,
      "search" -> search _,
      "all" -> allUsers _,
      "conversation" -> conversation _)

  def conversation(): Can[NodeSeq] =   {
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
              what.map(m =>
                <li>{formatMsg(m,
                               false, false,
                               Full(() =>
                        show(replyMap.getOrElse(m.id, Nil).reverse)
                      ))}</li>)
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

  def index(): Can[NodeSeq] =
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
                <dd class="b-view-menu clear">
                  <ul>
                    <li class="current"><b>Timeline</b></li>
                    <li><a href="javascript:" onclick="document.getElementById('tabs').className='b-view tab2';return false">Messages</a></li>
                    <li><a href="javascript:" onclick="document.getElementById('tabs').className='b-view tab3';return false">Contacts</a></li>
                  </ul>
                </dd>
                <dd>
                  {
                    val lst: List[Message] =
                    Mailbox.mostRecentMessagesFor(user.id, 50).map(_._1)
                    //  user.latestMessages(50) openOr Nil
                    lst.map(m => formatMsg(m, false, true))
                  }
                </dd>
              </dl>
		  
              <dl class="tagclouds">
                <dt class="caption">{user.niceName}'s Messages</dt>
               
                <dd class="b-view-menu clear">
                  <ul>
                    <li><a href="javascript:" onclick="document.getElementById('tabs').className='b-view tab1';return false">Timeline</a></li>
                    <li class="current"><b>Messages</b></li>
                    <li><a href="javascript:" onclick="document.getElementById('tabs').className='b-view tab3';return false">Contacts</a></li>
                  </ul>
                </dd>
                <dd class="b-clouds">
                  {
                    val lst: List[Message] =
                    Message.findAll(By(Message.author, user), MaxRows(50), OrderBy(Message.id, Descending))
                    lst.map(m => formatMsg(m, false, true))
                  }
                </dd>
              </dl>
              <dl class="contacts">
                <dt class="caption">
                  Contacts
                </dt>
                <dd class="b-view-menu clear">
                  <ul>
                    <li><a href="javascript:" onclick="document.getElementById('tabs').className='b-view tab1';return false">Timeline</a></li>
                    <li><a href="javascript:" onclick="document.getElementById('tabs').className='b-view tab2';return false">Messages</a></li>
                    <li class="current"><b>Contacts</b></li>
                  </ul>
                </dd>
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
          /*
           (<div>
           <fieldset>
           <legend>{user.niceName}</legend>
           <span id="following">{followOrNot}</span>
           <ul>
           {
           val lst: List[Message] =
           Mailbox.mostRecentMessagesFor(user.id, 50).map(_._1)
           //  user.latestMessages(50) openOr Nil
           lst.map(m => <li>{formatMsg(m, false, true)}</li>)
           }
           </ul>
           </fieldset>
           </div>)
           */
        }
        ui openOr (<span>User Not Found</span>)
      }
   </lift:surround>)
  
  def search(): Can[NodeSeq] =
  for (user <- User.currentUser;
       term <- S.param("term")) yield
  <lift:surround with="default" at="content">
    <div>
      <fieldset>
        <legend>Search: {term}</legend>
        <ul>
          {
            val lst: List[Message] = Message.search(term, user.following, 50)
            lst.map(m => <li>{formatMsg(m, false, true)}</li>)
          }
        </ul>
      </fieldset>
    </div>
  </lift:surround>
  
  def displayTag(): Can[NodeSeq] =
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
                    lst.map(m => <li>{formatMsg(m, false, true)}</li>)
                  }
                </ul>
              </fieldset>
           </div>)
        }
        ui openOr (<span>Tag Not Found</span>)
      }
   </lift:surround>)
  
  def allUsers(): Can[NodeSeq] = 
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
