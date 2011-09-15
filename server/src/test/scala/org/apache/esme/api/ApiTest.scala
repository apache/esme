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

package org.apache.esme.api

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import net.liftweb.common._
import org.specs.matcher._
import Helpers._                                   
import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.{Context, FilterHolder}
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.webapp.WebAppContext
import org.apache.esme._
import model._
import net.liftweb.http._
                                                        
import _root_.junit.framework.AssertionFailedError
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework, Response}

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


  private def testSuccess(resp: Response) {
    resp match {
      case resp: HttpResponse =>
        resp.code must_== 200
        resp.contentType.toLowerCase().startsWith("text/xml") must_== true
        (resp.xml.open_! \ "@success").text must_== "true"
      case _ => fail("Not an HTTP Response")
    }
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
        login <- post("/api/login", "token" -> token) // !@ "Failed to log in" if (testSuccess(login))
        status <- login.get("/api/status") // !@ "Failed to get status" if (testSuccess(status))
        otherStatus <- get("/api/status") if shouldnt(testSuccess(status))
      } {
        
        (status.xml.open_! \ "user" \ "@id").text must_== theUser.id.toString
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
        (status.xml.open_! \ "user" \ "@id").text must_== theUser.id.toString
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
        (status.xml.open_! \ "user" \ "@id").text must_== theUser.id.toString
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
      } {
        testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action")
        testSuccess(login.post("/api/add_action", "name" -> "1test", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a name starting with a number")
        testSuccess(login.post("/api/add_action", "name" -> "test1", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a name including a number")
        testSuccess(login.post("/api/add_action", "name" -> "test%", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a name including a punctuation")
        testSuccess(login.post("/api/add_action", "name" -> "t", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a short name with one character")
        testSuccess(login.post("/api/add_action", "name" -> "tttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt", "test" -> "#moo", "action" -> "filter") !@ "Failed to add action with a long name")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "login", "action" -> "filter") !@ "Failed to add 'login' action")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "followed", "action" -> "filter") !@ "Failed to add 'followed' action")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "unfollowed", "action" -> "filter") !@ "Failed to add 'unfollowed' action")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "filter") !@ "Failed to add 'profile' action")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "resend") !@ "Failed to add  action with a 'resend' action")
        //testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "mailto:foo@bar.com") !@ "Failed to add  action with a 'mail' action")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "http://foo.com/message/in") !@ "Failed to add  action with a 'http post' action")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "atom:http://blog.com/feed.atom") !@ "Failed to add  action with a 'atom' action")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "profile", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a 'rss' action")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "@api_test", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a '@[user} test'")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "resent:api_test", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a 'resent[:user} test'")
        testSuccess(login.post("/api/add_pool/ttt") !@ "Failed to add  a pool")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "pool:ttt", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a 'pool[:pool} test'")
        testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "every 7 mins", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a 'every 7 mins' test'")
   //      testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "50%", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with a '50%' test'")
         //testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "every 7 mins &amp; @api_test", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with multiple tests'")
         //testSuccess(login.post("/api/add_action", "name" -> "ttt", "test" -> "every 7 mins &amp; @api_test &amp; #moo", "action" -> "rss:http://blog.com/feed.rss") !@ "Failed to add  action with multiple tests'")

      }
    }
      
     "AddActionNeg" in {
      shouldnt(testSuccess(post("/api/add_action", "name" -> "test", "test" -> "#moo", "action" -> "filter") !@ "add_action should have failed: no login first"))
      for{
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
      } {
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "#moo") !@ "add_action should have failed: no action"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "action" -> "filter") !@ "add_action should have failed: no test"))
        shouldnt(testSuccess(login.post("/api/add_action", "action" -> "filter", "test" -> "#moo") !@ "add_action should have failed: no name"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "unkown", "action" -> "filter") !@ "add_action should have failed: unknown test"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "moo", "action" -> "unknown") !@ "add_action should have failed: unknown action"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "@foo", "action" -> "filter") !@ "add_action should have failed: unknown user"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "resent:foo", "action" -> "filter") !@ "add_action should have failed: unknown user"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "pool:foo", "action" -> "filter") !@ "add_action should have failed: unknown pool"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "every 0 mins", "action" -> "filter") !@ "add_action should have failed: no number"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "every -1 mins", "action" -> "filter") !@ "add_action should have failed: no number"))
        shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "every a mins", "action" -> "filter") !@ "add_action should have failed: no number"))
        //shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "110%", "action" -> "filter") !@ "add_action should have failed: no number"))
        //shouldnt(testSuccess(login.post("/api/add_action", "name" -> "test", "test" -> "j%", "action" -> "filter") !@ "add_action should have failed: no number"))

      }
    }

     "SendMessageToken" in {
      for{
        send_msg <- post("/api/send_msg", "token" -> token, "message" -> "mymessage") !@ "Failed to send_msg with token" if (testSuccess(send_msg))
       } { }
    }
    
    "AddPool" in {
      for{
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        add_pool <- login.post("/api/add_pool/ttt87") !@ "Failed to add  a pool" if (testSuccess(add_pool))
       } { }
    }

    "AddPoolNeg" in {
      for{
        add_pool <- post("/api/add_pool/ttt8787") !@ "add pool should have failed with no login" if shouldnt(testSuccess(add_pool))
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        add_pool1 <- login.post("/api/add_pool")
      } {
        add_pool1.code must be equalTo 404
      }
    }
  
    "SendMessageTokenNeg1" in {
      for{
        send_msg <- post("/api/send_msg", "token" -> "badtoken", "message" -> "mymessage") // !@ "send_msg should have failed with bad token" if shouldnt(testSuccess(send_msg))
      } {               
        send_msg.code must be equalTo 404
      }
    }
    "SendMessageTokenNeg" in {
      for{
        send_msg1 <- post("/api/send_msg", "token" -> token) !@ "send_msg should have failed with no message" if shouldnt(testSuccess(send_msg1))
       } { }
    }

  }


}
