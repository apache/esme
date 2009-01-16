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

import java.io.IOException
import java.text._
import java.util.Date

import org.apache.commons.httpclient.methods.GetMethod

import us.esme.actor.HttpSender
import us.esme.actor.UniqueMessageSource
import us.esme.actor.Distributor.UserCreatedMessage
import us.esme.model.User

import net.liftweb.util._

abstract class Feed(val user: User, val url: String, val source: String, val truncateChars: Int, val tags: List[String]) extends UniqueMessageSource {
  // import java.net.{URLConnection, URL}
  import scala.xml._

  protected def dateFormats: List[SimpleDateFormat]
  var dateFormat: SimpleDateFormat = _

  override def apply() = {
    
    ( for (node <- (getEntries(XML.loadString(responseString))))
        yield UserCreatedMessage(
          if (user != null) {user.id} else 0,
          getText(node) + " " + getLink(node),
          tags,
          getDate(node),
          Empty,
          source,
          Empty
        )
    ).toList
  }
  
  protected def getEntries(xml: Elem): NodeSeq
  
  protected def getText(xml: Node): String
  
  protected def getLink(xml: Node): String
  
  protected def getDate(xml: Node): Long
  
  protected def responseString() = {
    // url.openConnection
    val httpClient = HttpSender.httpClient
    val method = new GetMethod(url)
    try {
      httpClient.executeMethod(method)
      method.getResponseBodyAsString
    } catch {
      case ioe: IOException => ""
    } finally {
      method.releaseConnection
    }
  }
  
  protected def parseInternetDate(dateString: String): Date = {
    val fixedDateString = fixDateString(dateString)
    if (dateFormat == null) dateFormats.find { df =>
      try {
        dateFormat = df
        return df.parse(fixedDateString)
        // true
      } catch {
        case pe: ParseException => false
      }
    }
    dateFormat.parse(fixedDateString)
  }
  
  protected def fixDateString(dateString: String) = {
    val l = dateString.length
    if (dateString.charAt(l - 3) == ':')
      dateString.substring(0,l-3) + dateString.substring(l - 2, l)
    else dateString
  }

}

