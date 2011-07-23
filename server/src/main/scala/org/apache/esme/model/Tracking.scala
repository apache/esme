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
import Helpers._
import http._

import org.apache.esme._
import org.apache.esme.actor.Distributor

import scala.xml.{Node, Text, Elem}

object Tracking extends Tracking with LongKeyedMetaMapper[Tracking] {
  override def afterCommit = notifyDistributor _ :: super.afterCommit

  private def notifyDistributor(in: Tracking) {
    Distributor ! Distributor.UpdateTrackingFor(in.user, 
    Distributor.TrackTrackingType)
  }
}

class Tracking extends LongKeyedMapper[Tracking] {
  def getSingleton = Tracking // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)

  override def toXml: Elem = 
  <tracking id={id.toString} 
    user={user.box.map(l => Text(l.toString)).toOption}
    pattern={pattern}
    removed={removed.toString}
    createdAt={createdAt.toString}></tracking>

    object user extends MappedLongForeignKey(this, User)
    object removed extends MappedBoolean(this) {
      override def defaultValue = false
    }
    object createdAt extends MappedLong(this) {
      override def defaultValue = millis
    }
    object disabled extends MappedBoolean(this) {
      override def defaultValue = false
    }
    private[model] object what extends MappedString(this, 512) {
      override def validations = 
        valMinLen(10, "Description must be 10 characters") _ :: 
        valUnique("That action has already been taken") _ :: 
        super.validations
    }

object action extends MappedString(this, 2048)
  
    object who extends MappedLongForeignKey(this, User)

object uniqueId extends MappedUniqueId(this, 24) {
  override def dbIndexed_? = true
}


    def pattern: String = who.obj match {
      case Full(who) => "@"+who.niceName
      case _ => what.is
    }

    def regex(in: String): Tracking = {
      val i2 = in.trim
      if (i2.startsWith("@")) {
        who(User.find(By(User.nickname, i2.substring(1).trim)))
      }
      what(i2)
    }

    def matcher: Box[TrackingMatcher] = {
      who.box match {
        case Full(whoId) =>
          Full(new PersonTrackingMatcher(id, whoId))

        case _ =>
          if (what.startsWith("#"))
          Full(new TagTrackingMatcher(id, Tag.capify(what.substring(1))))
          else
          tryo(new RegexTrackingMatcher(id, what.is))
      }
    }
    }

    sealed trait TrackingMatcher extends Ordered[TrackingMatcher] {
      def doesMatch_?(in: Message) : Boolean
      def trackId: Long
      // def onReceipt: Boolean
    }

    case class PersonTrackingMatcher(trackId: Long, whoId: Long) extends TrackingMatcher {
      def doesMatch_?(in: Message): Boolean = in.author.is == whoId
      
      def compare(other: TrackingMatcher): Int = other match {
        case PersonTrackingMatcher(_, otherWho) => whoId.compare(otherWho)
        case _ => -1
      }
    }

    case class TagTrackingMatcher(trackId: Long, tag: String)
    extends TrackingMatcher {
      def doesMatch_?(in: Message): Boolean = in.tags.exists(_ == tag)
      def compare(other: TrackingMatcher): Int = other match {
        case PersonTrackingMatcher(_, _) => 1
        case TagTrackingMatcher(_, otherTag) => tag.compare(otherTag)
        case _ => -1
      }
    }

    case class RegexTrackingMatcher(trackId: Long, re: String) extends TrackingMatcher {
      private val regex = re.r
  
      def doesMatch_?(in: Message): Boolean =
      regex.findFirstIn(in.getText).isDefined
      
      def compare(other: TrackingMatcher): Int = other match {
        case RegexTrackingMatcher(_, otherRe) => re.compare(otherRe)
        case _ => 1
      }
    }


