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
      } {
        println(add_action.xml)
      }
    }
    
     "AddActionNeg" in {
      for{
        add_action <- post("/api/add_action", "name" -> "test", "test" -> "#moo", "action" -> "filter") !@ "add_action should have failed: no login first" if shouldnt(testSuccess(add_action))
        login <- post("/api/login", "token" -> token) !@ "Failed to log in" if (testSuccess(login))
        add_action1 <- login.post("/api/add_action", "name" -> "test", "test" -> "#moo") !@ "add_action should have failed: no action" if shouldnt(testSuccess(add_action1))
        add_action2 <- login.post("/api/add_action", "name" -> "test", "action" -> "filter") !@ "add_action should have failed: no test" if shouldnt(testSuccess(add_action2))
        add_action3 <- login.post("/api/add_action", "action" -> "filter", "test" -> "#moo") !@ "add_action should have failed: no name" if shouldnt(testSuccess(add_action3))
        add_action4 <- login.post("/api/add_action", "name" -> "test", "test" -> "moo", "action" -> "filter") !@ "add_action should have failed: no name" if shouldnt(testSuccess(add_action3))

      } {
        println(add_action3.xml)
      }
    }


  }


}
