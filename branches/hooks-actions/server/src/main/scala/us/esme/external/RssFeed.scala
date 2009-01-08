package us.esme.external

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

import java.text._
import java.util.Locale.US

import us.esme.model.User

object RssFeed {
  val dateFormats = List(new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", US))
}

class RssFeed(user: User, rssURL: String, source: String, truncateChars: Int, tags: List[String])
  extends Feed(user, rssURL, source, truncateChars, tags) {
  import scala.xml._
  
  override def dateFormats = RssFeed.dateFormats
  
  override def getEntries(xml: Elem) = xml \\ "item"
  
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
      parseInternetDate(date text).getTime
  }
  
}

