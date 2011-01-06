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

import java.io.IOException
import java.text._
import java.util.Date


import org.apache.commons.httpclient.methods.GetMethod

import org.apache.esme.actor._
import org.apache.esme.model.User

import org.apache.esme.actor.Distributor.UserCreatedMessage

import net.liftweb.util._
import net.liftweb.common._

abstract class Feed(val user: User, val url: String, val source: String, val truncateChars: Int, val tags: List[String]) extends UniqueMessageSource {
  // import java.net.{URLConnection, URL}
  import scala.xml._

  protected def dateFormats: List[SimpleDateFormat]
  // var dateFormat: SimpleDateFormat = _

  override def apply() = {
    
    for {
         xml <- PCDataXmlParser(responseString).toList
         node <- getEntries(xml)
    } yield UserCreatedMessage(
      if (user != null) {user.id} else 0,
      getText(node) + " " + getLink(node),
      tags,
      getDate(node),
      Empty,
      source,
      Empty,
      None
    )
  }
  
  protected def getEntries(xml: NodeSeq): NodeSeq
  
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
  
  protected def parseInternetDate(dateString: String): Option[Date] = {
    val fixedDateString = fixDateString(dateString)
    dateFormats.view.flatMap(df => Helpers.tryo {
      df.parse(fixedDateString)
    }).headOption
  }
  
  protected def fixDateString(dateString: String) = {
    val l = dateString.length
    if (dateString.charAt(l - 3) == ':')
      dateString.substring(0,l-3) + dateString.substring(l - 2, l)
    else dateString
  }

}

