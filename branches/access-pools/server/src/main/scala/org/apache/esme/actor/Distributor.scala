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

import scala.actors.Actor
import Actor._

import net.liftweb._
import http._
import util._

import org.apache.esme._
import model._

import scala.xml.{Elem}

object Distributor extends Actor {
  def act = loop {
    react {
           case RelinkToActorWatcher =>
        link(ActorWatcher)

      case StartMeUp =>
        link(ActorWatcher)
        User.findAll.map(_.id.is).foreach(findOrCreateUser)
        Group.findAll.map(_.id.is).foreach(findOrCreateGroup)

      case UserCreatedMessage(user, text, tags, when, 
                              metaData,
                              source,
                              inReplyTo,
                              pool) =>
        findOrCreateUser(user) ! 
        UserActor.CreateMessage(text, tags,
                                when, metaData, source, inReplyTo, pool)

      case AddMessageToMailbox(user, message, reason) =>
        findOrCreateUser(user) ! UserActor.AddToMailbox(message, reason)
  
      case Listen(user, who) =>
        findOrCreateUser(user) ! UserActor.Listen(who)

      case Unlisten(user, who) =>
        findOrCreateUser(user) ! UserActor.Unlisten(who)

      case LatestMessages(user, cnt) =>
        findOrCreateUser(user).forward(UserActor.LatestMessages(cnt))

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

      case _ =>
    }
  }

  private case object StartMeUp

  case class UserCreatedMessage(user: Long, text: String, tags: List[String],
                                when: Long,
                                metaData: Box[Elem],
                                source: String,
                                replyTo: Box[Long],
                                pool: Box[Long])
  case class AddMessageToMailbox(user: Long, message: Message, reason: MailboxReason)
  case class Listen(user: Long, who: Actor)
  case class Unlisten(user: Long, who: Actor)
  case class LatestMessages(user: Long, cnt: Int)
  case class NewMessage(msg: Message)
  case class UpdateTrackingFor(user: Long, which: TrackingType)
  case class UserUpdated(userId: Long)
  case class PublicTimelineListeners(who: Actor)
  case class PublicTimelineUnlisteners(who: Actor)
  case class AllowUserInPool(userId: Long, poolId: Long)
  sealed trait TrackingType
  case object PerformTrackingType extends TrackingType
  case object TrackTrackingType extends TrackingType

  start
  this ! StartMeUp

  // do nothing
  def touch {

  }

  private var users: Map[Long, UserActor] = Map.empty
  private var groups: Map[Long, GroupActor] = Map.empty
  private var listeners: List[Actor] = Nil

  private def findOrCreateUser(user: Long): UserActor = {
    users.get(user) match {
      case Some(ret) => ret
      case _ =>
        val ret = new UserActor
        ret.start
        ret ! UserActor.StartMeUp(user)
        users = users + (user -> ret)
        ret
    }
  }

  private def findOrCreateGroup(group: Long) {
    groups.get(group) match {
      case Some(ret) => ret
      case _ =>
        val ret = new GroupActor
        ret.start
        ret ! GroupActor.StartMeUp(group)
        groups = groups + (group -> ret)
        ret
    }
  }
}
