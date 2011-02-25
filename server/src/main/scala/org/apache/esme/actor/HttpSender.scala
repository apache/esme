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
import util._
import actor._
import common._
import net.liftweb.http.testing._

import org.apache.esme._
import model._
import lib._

import org.apache.commons.httpclient._
import org.apache.commons.httpclient.auth._
import methods._
import java.io.OutputStream

object HttpSender extends LiftActor with GetPoster with Loggable {
  protected def messageHandler = {
      case SendAMessage(action, msg, user, reason, token) =>
        send(action, msg, user, reason, token)

      case _ =>
  } 
 
  private case object StartMeUp
  case class SendAMessage(action: Performances, msg: Message, user: User, reason: MailboxReason, token: String)

  private def send(action: Performances, msg: Message, user: User, reason: MailboxReason, token: String) {
    import Mailer._
    
    (action: @unchecked) match {
      case MailTo(who, text) =>
        val body = text match {
          case None => XHTMLMailBodyType(msg.digestedXHTML)
          case Some(t) => PlainMailBodyType(expandText(t, msg, user, reason))
        }
        Mailer.sendMail(From("esme@esme.apache.org"), Subject("Message from ESME"),
                        To(who), body)
          
      case HttpTo(url, username, password, headers, data) =>
        val load = data match {
          case None => ""
          case Some(d) => expandText(d, msg, user, reason)
        }
        post(url, httpClient,
             ("X-ESME-Token" -> token) :: headers,
             username, password,
             load)
        
      case PerformResend | PerformFilter => // IGNORE
      
    }
  }
  
  private def expandText(text: String, msg: Message, user: User, reason: MailboxReason) = {
    val followerId = reason match {
      case FollowedReason(followerId) => Some(followerId)
      case UnfollowedReason(followerId) => Some(followerId)
      case _ => None
    }
    
    val followerName = followerId match {
      case Some(followerId) => User.find(followerId).map[String](_ nickname).openOr("N/A")
      case None => "N/A"
    }
    
    text.replace("%u", user.nickname).
    replace("%f", followerName).
    replace("%i", user.imageUrl).
    replace("%w", user.wholeName).
    replace("%s", msg.body).
    replace("%t", msg.getTags).
    replace("%d", msg.getWhen.toString)
  }

  // Overloaded method from GetPoster
  private def post(url: String, httpClient: HttpClient,
       headers: List[(String, String)],
       username: String, password: String,
       body: String) {
    val poster = new PostMethod(baseUrl + url)
    for ((name, value) <- headers) poster.setRequestHeader(name, value)
    poster.setRequestEntity(new RequestEntity {
      private val bytes = body.toString.getBytes("UTF-8")

      def getContentLength() = bytes.length
      def getContentType() = "application/x-www-form-urlencoded"
      def isRepeatable() = true
      def writeRequest(out: OutputStream) {
    out.write(bytes)
      }
    })

    httpClient.getState().setCredentials(AuthScope.ANY, new UsernamePasswordCredentials(username, password))
    poster.setDoAuthentication(true)

    try {
      httpClient.executeMethod(poster)
      logger.debug(poster.getStatusText)
      logger.debug(poster.getResponseBodyAsString)
    } finally {
      poster.releaseConnection
    }
  }
  
  def httpClient = {
    val ret = new HttpClient(new SimpleHttpConnectionManager())

    for (host <- Props.get("http.proxyHost"))
    ret.getHostConfiguration.setProxy(host,
                                      Props.getInt("http.proxyPort",80))
    ret
  }


  def baseUrl = ""
}
