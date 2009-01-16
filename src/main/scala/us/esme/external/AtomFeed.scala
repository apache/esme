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

object AtomFeed{
  val dateFormats = List(new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ", US),
                                  new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'", US),
                                  new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ", US),
                                  new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", US))
}

class AtomFeed(user: User, atomURL: String, source: String, truncateChars: Int, tags: List[String])
  extends Feed(user, atomURL, source, truncateChars, tags) {
  import scala.xml._
  
  override def dateFormats = AtomFeed.dateFormats
  
  override def getEntries(xml: Elem) = xml \ "entry"
  
  override def getText(node: Node) = {
    // a title element is mandatory
    node \ "title" text
  }
  
  override def getLink(node: Node) = {
    // there must be either a link with @rel="alternate"
    // or a link without @rel or content
    val link = node \ "link"
    if (link isEmpty)
      node \ "content" text
    else {
      val alternate = link find(_ \ "@rel" == "alternate")
      val anyLink = alternate getOrElse((link find(_ \ "@rel" == Nil)).get)
      anyLink \ "@href" text
    }
  }
  
  override def getDate(node: Node) = {
    // parseInternetDate(node \ "published" text).getTime
    val published = node \ "published"
    val date = if (published isEmpty)
      node \ "updated"
      else published
    parseInternetDate(date text).getTime
  }
}

