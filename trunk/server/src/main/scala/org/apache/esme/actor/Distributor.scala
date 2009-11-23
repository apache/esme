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
 * Distributor.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.apache.esme.actor

import net.liftweb._
import actor._
import http._
import util._
import common._

import org.apache.esme._
import model._

import scala.xml.{Elem}

/**
 * The Distributor actor forwards messages to the approprite user actor
 * without the need for the sender to have a reference to it.
 * If the actor is not started, Distributor starts it up in findOrCreateUser
 */
object Distributor extends LiftActor {
  protected def messageHandler = {
      case StartMeUp =>
        User.findAll.map(_.id.is).foreach(findOrCreateUser)

      case UserCreatedMessage(user, text, tags, when, 
                              metaData,
                              source,
                              inReplyTo,
                              pool) =>
        val toact = findOrCreateUser(user)
        toact ! UserActor.CreateMessage(text, tags,
                                        when, metaData, source, inReplyTo, pool)
        toact ! text

      case AddMessageToMailbox(user, message, reason) =>
        findOrCreateUser(user) ! UserActor.AddToMailbox(message, reason)
  
      case Listen(user, who) =>
        findOrCreateUser(user) ! UserActor.Listen(who)

      case Unlisten(user, who) =>
        findOrCreateUser(user) ! UserActor.Unlisten(who)

      case LatestMessages(user, cnt) =>
        forwardMessageTo(UserActor.LatestMessages(cnt), findOrCreateUser(user))

      case m @ UserUpdated(id) =>
        users.get(id).foreach(_ ! m)

      case nm @ NewMessage(msg) =>
        val toSend = UserActor.TestForTracking(msg)
        users.values.foreach(_ ! toSend)
        listeners.foreach(_ ! nm)

      case UpdateTrackingFor(userId, ttype) =>
        for (user <- users.get(userId))
        user ! UserActor.UpdateTracking(ttype)
          
      case PublicTimelineListeners(who) =>
        listeners = who :: listeners

      case PublicTimelineUnlisteners(who) =>
        listeners = listeners.filter(_ ne who)
        
      case AllowUserInPool(userId, poolId) =>
        findOrCreateUser(userId) ! UserActor.AllowPool(poolId)
        
      case RefreshUser(userId) =>
          users.get(userId).foreach(_ ! UserActor.RefreshMe(userId))
        
      case ResendMessage(userId, msgId) =>
        findOrCreateUser(userId) ! UserActor.Resend(msgId)

      case _ =>
  }

  private case object StartMeUp

  case class UserCreatedMessage(user: Long, text: String, tags: List[String],
                                when: Long,
                                metaData: Box[Elem],
                                source: String,
                                replyTo: Box[Long],
                                pool: Box[Long])
  case class AddMessageToMailbox(user: Long, message: Message, reason: MailboxReason)
  case class Listen(user: Long, who: LiftActor)
  case class Unlisten(user: Long, who: LiftActor)
  case class LatestMessages(user: Long, cnt: Int)
  case class NewMessage(msg: Message)
  case class UpdateTrackingFor(user: Long, which: TrackingType)
  case class UserUpdated(userId: Long)
  case class PublicTimelineListeners(who: LiftActor)
  case class PublicTimelineUnlisteners(who: LiftActor)
  case class AllowUserInPool(userId: Long, poolId: Long)
  case class RefreshUser(userId: Long)
  case class ResendMessage(userId: Long, msgId: Long)
  sealed trait TrackingType
  case object PerformTrackingType extends TrackingType
  case object TrackTrackingType extends TrackingType

  // do nothing
  def touch {

  }

  private var users: Map[Long, UserActor] = Map.empty
  private var listeners: List[LiftActor] = Nil

  private def findOrCreateUser(user: Long): UserActor = {
    users.get(user) match {
      case Some(ret) => ret
      case _ =>
        val ret = new UserActor
      // ret.start
        ret ! UserActor.StartMeUp(user)
        users = users + (user -> ret)
        ret
    }
  }

  def getUsersCount = users.size
  def getListenersCount = listeners.size

}
