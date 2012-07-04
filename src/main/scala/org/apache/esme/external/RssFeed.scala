package org.apache.esme.external

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

import java.text._
import java.util.Locale.US

import org.apache.esme.model.User
import org.apache.esme.model.Message
import org.apache.esme.actor.Distributor.{UserCreatedMessage=>Msg}

import net.liftweb.common.{Empty,Full,Box}

object RssFeed {
  val dateFormats = List(new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", US))
}

class RssFeed(user: User, rssURL: String, source: String, truncateChars: Int, tags: List[String])
  extends Feed(user, rssURL, source, truncateChars, tags) {
  import scala.xml._

  override def dateFormats = RssFeed.dateFormats
  
  override def getEntries(xml: NodeSeq) = xml \\ "item"
  
  override def getText(node: Node) = {
    // if there's no title, get description
    val title = node \ "title"
    if (title isEmpty)
      node \ "description" text
    else
      title text
  }
  
  override def getLink(node: Node) = {
    // a link is optional
    val link = node \ "link"
    if (link isEmpty)
      ""
    else
      link text
  }
  
  override def getDate(node: Node) = {
    // if there's no published date, take current time
    val date = node \ "pubDate"
    if (date isEmpty)
      System.currentTimeMillis
    else
      parseInternetDate(date text).map(_.getTime).getOrElse(System.currentTimeMillis)
  }
  
  // need to compare by text since a pubDate is not mandatory and indeed, often is missing
  /*
  override def getLastSortedMessages(msgs: List[Msg], lastMessage: Option[Msg]): List[Msg] = {
    lastMessage match {
      case Some(message: Msg) =>
        // a hack to format text identically- difference in urls & trailing whitespace
        val lastMessageText =
          Message.create.setTextAndTags(message.text, Nil, Empty).
            get.body.trim
        msgs.takeWhile{ msg =>
          Message.create.setTextAndTags(msg.text, Nil, Empty).
            get.body.trim != lastMessageText
        }
      case None => msgs
    }
  }.reverse
  */

  override def getLastSortedMessages(msgs: List[Msg], lastMessage: Option[Msg]): List[Msg] = {
    lastMessage match {
      case Some(message: Msg) =>
        // a hack to format text identically- difference in urls & trailing whitespace
        val lastMessageText =
          Message.create.setTextAndTags(message.text, Nil, Empty).
            choice((m: Message) => Full(m.body.trim))(Full("")).get
            //get.body.trim
        msgs.takeWhile{ msg =>
          Message.create.setTextAndTags(msg.text, Nil, Empty).
            choice((m: Message) => Full(m.body.trim))(Full("")).get != lastMessageText
            //get.body.trim != lastMessageText
        }
      case None => msgs
    }
  }.reverse
}

