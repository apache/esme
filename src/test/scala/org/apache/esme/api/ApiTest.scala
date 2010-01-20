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

package org.apache.esme.api

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import net.liftweb.common._
import org.specs.matcher._
import Helpers._
import net.sourceforge.jwebunit.junit.WebTester
import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.{Context, FilterHolder}
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.webapp.WebAppContext
import org.apache.esme._
import model._
import net.liftweb.http._

import net.sourceforge.jwebunit.junit.WebTester
import _root_.junit.framework.AssertionFailedError
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework}

class ApiSpecsAsTest extends JUnit3(ApiSpecs)
object ApiSpecsRunner extends ConsoleRunner(ApiSpecs)

object ApiSpecs extends Specification with TestKit {
  JettyTestServer.start

  val baseUrl = JettyTestServer.urlFor("")

  implicit val reportError = new ReportFailure {
    def fail(msg: String): Nothing = ApiSpecs.this.fail(msg)
  }

  val session = new LiftSession(Helpers.randomString(20), "", Empty)

  val theUser = S.initIfUninitted(session) {User.createAndPopulate.nickname("api_test").saveMe}

  val token = {
    val toke = AuthToken.create.user(theUser).saveMe
    toke.uniqueId.is
  }


  private def testSuccess(resp: HttpResponse) {
    resp.code must_== 200
    resp.contentType.toLowerCase().startsWith("text/xml") must_== true
    (resp.xml \ "@success").text must_== "true"
  }

  def shouldnt(f: => Unit): Unit =
    try {
      val x = f
      fail("Shouldn't succeed")
    } catch {
      case _ => ()
    }

  "API" should {

    "Login" in {
      for{
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        status <- login.get("/api/status") !@ "Failed to get status" if (testSuccess(status))
        otherStatus <- get("/api/status") if shouldnt(testSuccess(status))
      } {
        
        (status.xml \ "user" \ "@id").text must_== theUser.id.toString
      }
    }
    
     "LoginNeg" in {
      for{
        login <- post("/api/login", "token" -> "00000000") !@ "Login should have failed: bad token" if shouldnt(testSuccess(login))
        login2 <- post("/api/login", "token" -> "") !@ "Login should have failed: empty token" if shouldnt(testSuccess(login2))
      } {
       }
    }
      
    
    "StatusNeg" in {
      for{
        status <- get("/api/status") !@ "Status should have failed: no login" if shouldnt(testSuccess(status))
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        logout <- login.get("/api/logout") !@ "Failed to log out" if (testSuccess(logout))
        status2 <- login.get("/api/status") !@ "Status should have failed: no active login" if shouldnt(testSuccess(status2))
      } {
      }
    }
    
    "Logout" in {
      for{
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        status <- login.get("/api/status") !@ "Failed to get status" if (testSuccess(status))
        logout <- login.get("/api/logout") !@ "Failed to log out" if (testSuccess(status))
      } {
        (status.xml \ "user" \ "@id").text must_== theUser.id.toString
      }
    }
    
    "LogoutNeg" in {
      for{
         logout <- get("/api/logout") !@ "Status should have failed: no login" if shouldnt(testSuccess(logout))
         login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
         logout2 <- login.get("/api/logout") !@ "Failed to log out" if (testSuccess(logout2))
         logout3 <- login.get("/api/logout") !@ "logout should have failed: no active login" if shouldnt(testSuccess(logout3))

      } {
      }
    }
    "SendMsg" in {
      for{
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        status <- login.get("/api/status") !@ "Failed to get status" if (testSuccess(status))
        send_msg <- login.post("/api/send_msg", "message" -> "hi") !@ "Failed to send message" if (testSuccess(send_msg))
      } {
        (status.xml \ "user" \ "@id").text must_== theUser.id.toString
      }
    }
    "SendMsgNeg" in {
      for{
        send_msg <- post("/api/send_msg", "message" -> "Bad") !@ "send_msg should have failed: no login first" if shouldnt(testSuccess(send_msg))
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        send_msg2 <- login.post("/api/send_msg", "message" -> "") !@ "send_msg should have failed: empty message" if shouldnt(testSuccess(send_msg2))
      } {
      }
    }
    "AddAction" in {
      for{
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        add_action <- login.post("/api/add_action", "name" -> "test", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action" if (testSuccess(add_action))
        add_action1 <- login.post("/api/add_action", "name" -> "1test", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a name starting with a number" if (testSuccess(add_action1))
        add_action2 <- login.post("/api/add_action", "name" -> "test1", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a name including a number" if (testSuccess(add_action2))
        add_action3 <- login.post("/api/add_action", "name" -> "test%", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a name including a punctuation" if (testSuccess(add_action3))
        add_action4 <- login.post("/api/add_action", "name" -> "t", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a short name with one character" if (testSuccess(add_action4))
        add_action5 <- login.post("/api/add_action", "name" -> "tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a long name" if (testSuccess(add_action5))
        add_action6 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "login", "action" -> "filter") !@ "Failed to add 'login' action" if (testSuccess(add_action6))
        add_action7 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "followed", "action" -> "filter") !@ "Failed to add 'followed' action" if (testSuccess(add_action7))
        add_action8 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "unfollowed", "action" -> "filter") !@ "Failed to add 'unfollowed' action" if (testSuccess(add_action8))
        add_action9 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "filter") !@ "Failed to add 'profile' action" if (testSuccess(add_action9))
        add_action10 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "resend") !@ "Failed to add  action with a 'resend' action" if (testSuccess(add_action10))
        //add_action11 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "mailto:foo@bar.com") !@ "Failed to add  action with a 'mail' action" if (testSuccess(add_action11))
        add_action12 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "http://foo.com/message/in") !@ "Failed to add  action with a 'http post' action" if (testSuccess(add_action12))
        add_action13 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "atom:http://blog.com/feed.atom") !@ "Failed to add  action with a 'atom' action" if (testSuccess(add_action13))
        add_action14 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a 'rss' action" if (testSuccess(add_action14))
        add_action15 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "@api_test", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a '@[user} test'" if (testSuccess(add_action15))
        add_action16 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "resent:api_test", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a 'resent[:user} test'" if (testSuccess(add_action16))
        add_pool <- login.post("/api/add_pool/ttt") !@ "Failed to add  a pool" if (testSuccess(add_pool))
        add_action17 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "pool:ttt", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a 'pool[:pool} test'" if (testSuccess(add_action17))
        add_action18 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "every 7 mins", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a 'every 7 mins' test'" if (testSuccess(add_action18))
   //      add_action19 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "50%", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a '50%' test'" if (testSuccess(add_action19))
         //add_action20 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "every 7 mins &amp; @api_test", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with multiple tests'" if (testSuccess(add_action20))
         //add_action21 <- login.post("/api/add_action", "name" -> "ttt", "test" -> "every 7 mins &amp; @api_test &amp; #moo", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with multiple tests'" if (testSuccess(add_action21))

         } {
         println(add_pool.xml)
      }
    }
      
     "AddActionNeg" in {
      for{
        add_action <- post("/api/add_action", "name" -> "test", "test" -> "#moo", "action" -> "filter") !@ "add_action should have failed: no login first" if shouldnt(testSuccess(add_action))
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        add_action1 <- login.post("/api/add_action", "name" -> "test", "test" -> "#moo") !@ "add_action should have failed: no action" if shouldnt(testSuccess(add_action1))
        add_action2 <- login.post("/api/add_action", "name" -> "test", "action" -> "filter") !@ "add_action should have failed: no test" if shouldnt(testSuccess(add_action2))
        add_action3 <- login.post("/api/add_action", "action" -> "filter", "test" -> "#moo") !@ "add_action should have failed: no name" if shouldnt(testSuccess(add_action3))
        add_action4 <- login.post("/api/add_action", "name" -> "test", "test" -> "unkown", "action" -> "filter") !@ "add_action should have failed: unknown test" if shouldnt(testSuccess(add_action4))
        add_action5 <- login.post("/api/add_action", "name" -> "test", "test" -> "moo", "action" -> "unknown") !@ "add_action should have failed: unknown action" if shouldnt(testSuccess(add_action5))
        add_action6 <- login.post("/api/add_action", "name" -> "test", "test" -> "@foo", "action" -> "filter") !@ "add_action should have failed: unknown user" if shouldnt(testSuccess(add_action6))
        add_action7 <- login.post("/api/add_action", "name" -> "test", "test" -> "resent:foo", "action" -> "filter") !@ "add_action should have failed: unknown user" if shouldnt(testSuccess(add_action7))
        add_action8 <- login.post("/api/add_action", "name" -> "test", "test" -> "pool:foo", "action" -> "filter") !@ "add_action should have failed: unknown pool" if shouldnt(testSuccess(add_action8))
        add_action9 <- login.post("/api/add_action", "name" -> "test", "test" -> "every 0 mins", "action" -> "filter") !@ "add_action should have failed: no number" if shouldnt(testSuccess(add_action9))
        add_action10 <- login.post("/api/add_action", "name" -> "test", "test" -> "every -1 mins", "action" -> "filter") !@ "add_action should have failed: no number" if shouldnt(testSuccess(add_action10))
        add_action11 <- login.post("/api/add_action", "name" -> "test", "test" -> "every a mins", "action" -> "filter") !@ "add_action should have failed: no number" if shouldnt(testSuccess(add_action10))
        //add_action12 <- login.post("/api/add_action", "name" -> "test", "test" -> "110%", "action" -> "filter") !@ "add_action should have failed: no number" if shouldnt(testSuccess(add_action10))
        //add_action13 <- login.post("/api/add_action", "name" -> "test", "test" -> "j%", "action" -> "filter") !@ "add_action should have failed: no number" if shouldnt(testSuccess(add_action10))

      } {
        println(add_action7.xml)
        println(add_action8.xml)
      }
    }

     "SendMessageToken" in {
      for{
        send_msg <- post("/api/send_msg", "token" -> token, "message" -> "mymessage") !@ "Failed to send_msg with token" if (testSuccess(send_msg))
       } {
        println(send_msg.xml)
      }
    }
    
    "AddPool" in {
      for{
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        add_pool <- login.post("/api/add_pool/ttt87") !@ "Failed to add  a pool" if (testSuccess(add_pool))
       } {
        //println(add_pool.xml)
      }
    }
     "AddPoolNeg" in {
      for{
        add_pool <- post("/api/add_pool/ttt8787") !@ "add pool should have failed with no login" if shouldnt(testSuccess(add_pool))
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        add_pool1 <- login.post("/api/add_pool") !@ "add pool should have failed with no pool name" if shouldnt(testSuccess(add_pool1))
       } {
        println(add_pool.xml)
      }
    }
  
    "SendMessageTokenNeg1" in {
      for{
        send_msg <- post("/api/send_msg", "token" -> "badtoken", "message" -> "mymessage") !@ "send_msg should have failed with bad token" if shouldnt(testSuccess(send_msg))
       } {
        println(send_msg.xml)
      }
    }
    "SendMessageTokenNeg" in {
      for{
        send_msg1 <- post("/api/send_msg", "token" -> token) !@ "send_msg should have failed with no message" if shouldnt(testSuccess(send_msg1))
       } {
        println(send_msg1.xml)
      }
    }

  }


}
