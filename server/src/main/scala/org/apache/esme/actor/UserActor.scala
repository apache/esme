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

package org.apache.esme.actor

import net.liftweb._
import http._
import actor._
import util._
import common._
import mapper._

import org.apache.esme._
import model._
import lib._

import java.util.{TimeZone, Calendar}
import scala.xml.{Node, Elem}
//import com.twitter.stats.Stats
import com.twitter.ostrich.stats.Stats

object UserActor {
  private[actor] case class StartMeUp(user: Long)
  private[actor] case class RefreshMe(user: Long)
  private[actor] case class CreateMessage(text: String, tags: List[String],     
                                          when: Long, metaData: Box[Node],
                                          source: String,
                                          replyTo: Box[Long],
                                          pool: Box[Long])
  private[actor] case class AddToMailbox(msg: Message, reason: MailboxReason)
  private[actor] case class Listen(who: LiftActor)
  private[actor] case class Unlisten(who: LiftActor)
  private[actor] case class LatestMessages(cnt: Int)
  private[actor] case class TestForTracking(msg: Message)
  private[actor] case class UpdateTracking(ttype: Distributor.TrackingType)
  private[actor] case class AllowPool(poolId: Long)
  
  case class Resend(msgId: Long)
  case class MessageReceived(msg: Message, reason: MailboxReason)
  
  val logger: Logger = Logger("org.apache.esme.actor")
}


/**
 * The UserActor processes a user's messages
 * The UserActor keeps track of pools a user belongs to, followers,
 * active actions and tracking filters
 */
 
class UserActor extends LiftActor {
  import UserActor._
  
  private var userId: Long = 0

  private var userTimezone: TimeZone = _
  
  private var listeners: List[LiftActor] = Nil
  
  private var tracking: List[TrackingMatcher] = Nil

  private var perform: List[PerformMatcher] = Nil
  
  private var _mailbox: Array[(Long,MailboxReason,Boolean)] = Array()
  
  private var pools: List[Long] = Nil

  private def followers: List[Long] = User.followerIdsForUserId(userId)
  
  private def canReadPool_?(poolId: Long) = pools contains poolId  

  protected def messageHandler = {
      case m @ Distributor.UserUpdated(_) =>
        User.find(userId).
        foreach(u => userTimezone = TimeZone.getTimeZone(u.timezone))
        listeners.foreach(_ ! m)

      case StartMeUp(user) =>
        userId = user
        User.find(userId).
        foreach(u => userTimezone = TimeZone.getTimeZone(u.timezone))

        _mailbox = Mailbox.mostRecentMessagesFor(userId, 500).
        map{case (msg, reason, resent) => (msg.id.is, reason, resent) }.toArray
        
        pools = Privilege.findViewablePools(userId)

        this ! UpdateTracking(Distributor.TrackTrackingType)
        this ! UpdateTracking(Distributor.PerformTrackingType)

        Stats incr "userCount"
        
      case RefreshMe(user) => 
         pools = Privilege.findViewablePools(user)
        
      case CreateMessage(text, tags, when, metaData, source, replyTo, pool) =>
        val tagLst = tags.distinct.map(Tag.findOrCreate)

        Message.create.author(userId).when(when).
        source(source).
        setTextAndTags(text, tagLst, metaData).filter{ m =>
          pool match {
            case Full(p) => 
              m.pool(p)
              Privilege.hasPermission(userId, p, Permission.Write)
            case _ => true
          }
        }.map{msg =>
          // do some security... only reply to messages
          // that are in our mailbox
          for (rt <- replyTo;
               mb <- Mailbox.find(By(Mailbox.message, rt),
                                  By(Mailbox.user, userId));
               rtm <- Message.find(mb.message.is)) 
                 if (rtm.pool == msg.pool) msg.replyTo(rt)

          msg.saveMe           

          Stats incr "userMessagesCreated"
          Stats incr "messagesCreated"

          Distributor ! Distributor.AddMessageToMailbox(userId, msg, NoReason)
        
          for (id <- followers)
          Distributor ! Distributor.AddMessageToMailbox(id, msg, NoReason)
        
          for (id <- msg.sentToIds)
          Distributor ! Distributor.AddMessageToMailbox(id, msg,
                                                        DirectReason(userId))

          for (convId <- msg.conversation.box ;
               val msgId = Message.findMap(By(Message.conversation, convId))
               (m => Full(m.id.is));
               userId <- (Mailbox.findMap(InRaw(Mailbox.message, msgId.mkString(", "),
                                                IHaveValidatedThisSQL("dpp", "Aug 27. 2008")))
                          (mb => Full(mb.user.is))).distinct)
          Distributor ! Distributor.AddMessageToMailbox(userId, msg, ConversationReason(convId))
                                             
          Distributor ! Distributor.NewMessage(msg)     

		      reply(msg)  
        }

      case AddToMailbox(msg, reason) =>
        if (!msg.pool.defined_? || canReadPool_?(msg.pool.is))
          addToMailbox(msg, reason)
        
        
      case TestForTracking(msg) =>
        if (!msg.pool.defined_? || canReadPool_?(msg.pool.is))
          for (t <- tracking.find(_.doesMatch_?(msg)))
          this ! AddToMailbox(msg, TrackReason(t.trackId))
         
          
      case UpdateTracking(ttype) =>
        ttype match {
          case Distributor.TrackTrackingType =>
            tracking = Tracking.findAll(By(Tracking.user, userId),
                                        By(Tracking.disabled, false),
                                        By(Tracking.removed, false)).
            flatMap(_.matcher)
        
          case Distributor.PerformTrackingType =>
            perform = Action.findAll(By(Action.user, userId),
                                     By(Action.disabled, false),
                                     By(Action.removed, false)).
            map(_.matcher)
        }
  
      case Listen(who) => listeners = who :: listeners
    
      case Unlisten(who) => listeners = listeners.filter(_ ne who)
    
      case LatestMessages(cnt) =>
        reply(_mailbox.take(cnt).toList)
      
      case AllowPool(poolId) => pools ::= poolId
      
      case Resend(msgId) =>
        for (msg <- Message.find(msgId)) {
          if (!msg.pool.defined_?)
            PopStatsActor ! PopStatsActor.IncrStats(ResendStat, msgId)
            
          Mailbox.find(By(Mailbox.message, msg),
                       By(Mailbox.user, userId)).foreach { m =>
                         m.resent(true).save
                         _mailbox = _mailbox.map {
                           case (`msgId`, r, _) => (msgId, r, true)
                           case x => x
                         }
                         listeners.foreach(_ ! Resend(msgId))
                       }
          for (id <- followers)
            Distributor !
            Distributor.AddMessageToMailbox(id, msg, ResendReason(userId))
        }
  }

  def buildCalendar = userTimezone match {
    case null => Calendar.getInstance()
    case tz => Calendar.getInstance(tz)
  }
  
  /**
   * This method decides which actions should be applied to a message
   */
  private def addToMailbox(msg: Message, reason: MailboxReason) {
    // if the message is not in my mailbox
    if (Mailbox.find(By(Mailbox.message, msg),
                     By(Mailbox.user, userId)).isEmpty) {
      
      // get all the performance things
      val cal = buildCalendar
      val toDo = perform.filter(_.func(msg, userId, cal, reason))

      // is one of those reasons rejection of the message
      val reject = toDo.exists(_.filter_?)

      // if we're not rejecting the message
      if (!reject) {
        val mb = Mailbox.create.user(userId).message(msg)
        reason match {
          case TrackReason(trackId) => mb.viaTrack(trackId)
              Stats incr "messagesDeliveredTrackReason"
          case DirectReason(fromId) => mb.directlyFrom(fromId)
              Stats incr "messagesDeliveredDirectReason"
          case ConversationReason(convId) => mb.conversation(convId)
              Stats incr "messagesDeliveredConversationReason"
          case ResendReason(resender) => mb.resentBy(resender)
              Stats incr "messagesDeliveredResendReason"
          case _ =>
        }
        mb.saveMe
        Stats incr "messagesDelivered"

        // Only add to mailbox and notify listeners if there is a real message involved
        if(msg.id.is != -1) {
          _mailbox = ((msg.id.is, reason, false) :: _mailbox.toList).take(500).toArray
          listeners.foreach(_ ! MessageReceived(msg, reason)) 
        }                               
            
        toDo.foreach {
          td =>

          td.whatToDo match {
            case m @ MailTo(_, _) =>
              User.find(userId).foreach( u =>
                HttpSender ! HttpSender.SendAMessage(m, msg, u, reason, td.uniqueId))
                Stats incr "messagesMailed"
            case h @ HttpTo(_, _, _, _, _) =>
              User.find(userId).foreach( u =>
                HttpSender ! HttpSender.SendAMessage(h, msg, u, reason, td.uniqueId))
                Stats incr "messagesSentViaHTTP"
            case PerformResend =>
              if (! msg.saved_?) msg.save
              for (id <- followers)
              Distributor !
              Distributor.AddMessageToMailbox(id, msg, ResendReason(userId))

            case FetchFeed(_, _) => 
              MessagePullActor ! MessagePullActor.Fetch(td.performId)    

            case ScalaInterpret => logger.info("Scala interpreter is disabled!")
            /*if (msg.source.is != "scala")
              ScalaInterpreter ! ScalaInterpreter.ScalaExcerpt(userId, msg.id.is, msg.pool.is, msg.body)
            */

            case PerformFilter => Stats incr "messagesFiltered" // IGNORE
          }
        }
      }
    }
  }
}
