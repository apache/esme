package us.esme.model

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


import net.liftweb._
import mapper._
import util._

import scala.xml._

object Mailbox extends Mailbox with LongKeyedMetaMapper[Mailbox] {
  override def dbTableName = "mailbox" // define the DB table name

  def mostRecentMessagesFor(userId: Long, cnt: Int):
  List[(Message, MailboxReason)] = {
    val mb = findAll(By(user, userId), OrderBy(id, Descending),
                     MaxRows(cnt))

    val msgToFind: List[Long] = mb.map(_.message.is)

    val map = Message.findMessages(msgToFind)

    mb.flatMap(m => map.get(m.message).map(msg => (msg, m.reason)))
  }
    
  override def dbIndexes = Index(user, message) :: super.dbIndexes
}

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

  lazy val reason: MailboxReason =
  viaTrack.can.map(TrackReason) or directlyFrom.can.map(DirectReason)  or
  conversation.can.map(ConversationReason) openOr NoReason
}

sealed trait MailboxReason {
  def attr: MetaData
}
case class ResendReason(fromUserId: Long) extends MailboxReason {
  def attr = new UnprefixedAttribute("resent_from", fromUserId.toString, Null)
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
