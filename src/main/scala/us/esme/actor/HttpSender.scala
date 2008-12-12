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



package us.esme.actor

import scala.actors.Actor
import Actor._

import net.liftweb._
import http._
import util._
import net.liftweb.http.testing._

import us.esme._
import model._
import lib._

import org.apache.commons.httpclient._

object HttpSender extends Actor with GetPoster {
  def act = loop {
    react {
      case StartMeUp =>
        link(ActorWatcher)
        

      case SendAMessage(action, msg, token) =>
        send(action, msg, token)

      case _ =>
    }
  }

  private case object StartMeUp
  case class SendAMessage(action: Performances, msg: Message, token: String)

  private def send(action: Performances, msg: Message, token: String) {
    import Mailer._
    
    action match {
      case MailTo(who) => Mailer.sendMail(From("i@esme.us"), Subject("msg"),
                                          To(who),
                                          XHTMLMailBodyType(msg.digestedXHTML))
      case HttpTo(url, headers) =>
        post(url, httpClient,
             ("X-ESME-Token" -> token) :: headers,
             msg.toXml)
        
      case PerformResend | PerformFilter => // IGNORE
      
    }
  }
  
  def httpClient = {
    val ret = new HttpClient(new SimpleHttpConnectionManager(false))

    for (host <- Props.get("http.proxyHost"))
    ret.getHostConfiguration.setProxy(host,
                                      Props.getInt("http.proxyPort",80))
    ret
  }

  start
  this ! StartMeUp

  def baseUrl = ""
}
