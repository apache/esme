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
import util._
import common._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsExp

import scala.xml._

object Mailbox extends Mailbox with LongKeyedMetaMapper[Mailbox] {
  override def dbTableName = "mailbox" // define the DB table name

  /**
   * A list of the last messages are collected through the cached 
   * Message.findMessages method
   */
  def mostRecentMessagesFor(userId: Long, cnt: Int):
  List[(Message, MailboxReason, Boolean)] = {
    val mb = findAll(By(user, userId), OrderBy(id, Descending),
                     MaxRows(cnt))

    val msgToFind: List[Long] = mb.map(_.message.is)

    val map = Message.findMessages(msgToFind)

    mb.flatMap(m => map.get(m.message).map(msg => (msg, m.reason, m.resent.is)))
  }
    
  override def dbIndexes = Index(user, message) :: super.dbIndexes
}

/**
 * The Mailbox is a list of references to message instances,
 * which are in the user's timeline, along with additional data:
 * why the message got to this timeline and any conversation it's part of
 *
 * The Message itself is never copied when it's put in a user's timeline:
 * only another reference to it is created
 */
class Mailbox extends LongKeyedMapper[Mailbox] {
  def getSingleton = Mailbox // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object user extends MappedLongForeignKey(this, User)
  object message extends MappedLongForeignKey(this, Message)
  object viaTrack extends MappedLongForeignKey(this, Tracking)
  object directlyFrom extends MappedLongForeignKey(this, User)
  object conversation extends MappedLongForeignKey(this, Message)
  object resentBy extends MappedLongForeignKey(this, User)
  object resent extends MappedBoolean(this)

  lazy val reason: MailboxReason =
  viaTrack.box.map(TrackReason) or directlyFrom.box.map(DirectReason)  or
  conversation.box.map(ConversationReason) openOr NoReason
}

sealed trait MailboxReason {
  def attr: MetaData
  def asJs = attr match {
    case Null => JsNull
    case _ => JsObj((attr.key, attrValueAsJs(attr.value)))
  }
  def attrValueAsJs(value: Seq[Node]): JsExp = Str(value.toString)
}

case class ResendReason(fromUserId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("resent_from", fromUserId.toString, Null)
  
  override def attrValueAsJs(value: Seq[Node]) =
    User.find(fromUserId).map(_.asJs) openOr JsNull
}
case object NoReason extends MailboxReason {
  def attr = Null
}
case class TrackReason(trackId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("track", trackId.toString, Null)
}
case class DirectReason(fromUserId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("direct", fromUserId.toString, Null)
}
case class ConversationReason(conversationId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("conversation", conversationId.toString, Null)
}                                                                                 
case class TagFollowReason(tagName: String) extends MailboxReason {
  def attr = new UnprefixedAttribute("tag", tagName, Null)
}            
case class ConvFollowReason(convId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("conversation", convId.toString, Null)
}
case class LoginReason(userId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("login", userId.toString, Null)
}
case class FollowedReason(userId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("followed", userId.toString, Null)
}
case class UnfollowedReason(userId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("unfollowed", userId.toString, Null)
}
case class ProfileReason(userId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("profile", userId.toString, Null)
}
case class RegularReason(actionId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("regular", actionId.toString, Null)
}
case class InterpreterReason(userId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("interpreter", userId.toString, Null)
}
