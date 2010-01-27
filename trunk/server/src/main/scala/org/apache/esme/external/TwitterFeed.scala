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

import org.apache.esme.actor.UniqueMessageSource
import org.apache.esme.actor.Distributor.UserCreatedMessage
import org.apache.esme.model.User

import net.liftweb._
import util._
import common._

object TwitterFeed {
  val UserTimelineUrl = "http://twitter.com/statuses/user_timeline/"
  val Protocol = "xml"
  val Params = ""
  val tf = new java.text.SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy", java.util.Locale.US)
  
}

class TwitterFeed(val user: User, val twitterUser: String) extends UniqueMessageSource {
  import java.net.{URLConnection, URL}
  import scala.xml._
  import TwitterFeed._

  override def apply() = {
    val url = new URL(UserTimelineUrl + twitterUser + "." + Protocol + Params)
    val conn = url.openConnection()

    
    (for (node <- (XML.load(conn.getInputStream) \ "status")) yield UserCreatedMessage(
      user.id,
      node \ "text" text,
      List("twitter"),
      tf.parse(node \ "created_at" text).getTime,
      Empty,
      "twiiter",
      Empty,
      None
    )).toList
  }
  
}

