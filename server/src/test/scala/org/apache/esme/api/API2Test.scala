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
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework}

import net.sourceforge.jwebunit.junit.WebTester
import _root_.junit.framework.AssertionFailedError

class Api2SpecsAsTest extends JUnit3(Api2Specs)
object Api2SpecsRunner extends ConsoleRunner(Api2Specs)

object Api2Specs extends Specification with TestKit {
  JettyTestServer.start

  val baseUrl = JettyTestServer.urlFor("/api2/")  

  // Note "api_test" user is special. It has been set up with the integration-admin
  // role in the test.default.props file.
                                                                    
  val theUser = find_or_create_user("api_test")    
  val token = {
    val toke = AuthToken.create.user(theUser).saveMe
    toke.uniqueId.is
  }           

  val post_session = post("session", "token" -> token) 

  def find_or_create_user(userName: String): User = {
    val users = User.findByNickname(userName)

    if(users.length > 0)
      users.head
    else { 
      val session = new LiftSession(Helpers.randomString(20), "", Empty)                          
      S.initIfUninitted(session) {User.createAndPopulate.nickname(userName).saveMe}
    }
  }

  def sleep(wait: Long): Box[Boolean] = {
    Thread.sleep(wait)
    Full(true)
  }

  "API2" should {
    "/session POST" in {
      "Attempt to log in with a valid token should succeed with a 200 response" in {
        for{
          session <- post_session
        } {
          (session.xml \ "session" \ "user" \ "id").text must be equalTo (theUser.id.toString)
          (session.xml \ "session" \ "user" \ "nickname").text must be equalTo (theUser.niceName) 
          session.code must be equalTo 200
        }
      }

      "Attempt to create session with an invalid token returns 403 forbidden response" in {
        for{
          session <- post("session", "token" -> "000000")
        } {
          session.code must be equalTo 403
        }
      }
    }

    "/session GET" in {
      "with valid session" in {
        for{
          session <- post_session
          session_response <- session.get("session")
        } {
          session_response.code must be equalTo 200
          (session_response.xml \ "session" \ "user" \ "id").text must be equalTo (theUser.id.toString)
        }
      }

      "with no session returns 404 (not found)" in {
        for (session_res <- get("session")) {
          session_res.code must be equalTo 404
        }
      }
    }

    "/session DELETE" in {
      "with valid session" in {
        for{
          session <- post_session
          session_del_response <- session.delete("session")
          //session_response <- session.get("session")
        } {
          session_del_response.code must be equalTo 200
          //session_response.code must be equalTo(404)
        }
      }

      "with no session returns 404 (not found)" in {
        for (session_res <- delete("session")) {
          session_res.code must be equalTo 404
        }
      }
    }

    "/users GET" in {
      "with valid session" in {
        for{
          session <- post_session
          users <- session.get("users")
        } {                         
          users.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("users")) {
          session_res.code must be equalTo 403
        }
      }
    } 

    "/users POST" in {
      "with valid session" in {
        for{
          session <- post_session        
          added_user <- session.post("users",
            "nickname" -> "test_user5",
            "password" -> "test_password")  
          all_users <- session.get("users") 
        } {                                                                                                        
          added_user.code must be equalTo 200
          (all_users.xml \ "users") must \\(<nickname>test_user5</nickname>)
        }
      }

      "with a valid session but no role authorization returns 403 (forbidden)" in {
		val new_user = find_or_create_user("tester")
		val new_toke = AuthToken.create.user(new_user).saveMe
		val new_token = new_toke.uniqueId.is        
		
        for{     
		  sess <- post("session", "token" -> new_token)
		  added_user <- sess.post("users",
            "nickname" -> "test_user3",
            "password" -> "test_password")
        } {                 
          added_user.code must be equalTo 403
        }
      } 

      "with no session returns 403 (forbidden)" in {
        for{
          added_user <- post("users",
            "nickname" -> "test_user",
            "password" -> "test_password")
        } {
          added_user.code must be equalTo 403
        }
      }
    }

    "/users/USERID/tokens GET" in {
      val new_user = find_or_create_user("tester")
	  val new_toke = AuthToken.create.user(new_user).saveMe
	  val new_token = new_toke.uniqueId.is

      "with valid session" in {
        for{
          session <- post_session          
          tokens <- session.get("users/"+new_user.id+"/tokens") 
        } {                                                                                                        
          tokens.code must be equalTo 200
          tokens.xml must \\(<id>{new_token}</id>)
        }
      }

      "with valid session but no role authorization returns 403 (forbidden)" in {
		val new_user = find_or_create_user("tester")
		val new_toke = AuthToken.create.user(new_user).saveMe
		val new_token = new_toke.uniqueId.is        
		
        for{     
		  sess <- post("session", "token" -> new_token)
		  tokens <- sess.get("users/"+theUser.id+"/tokens")
        } {                 
          tokens.code must be equalTo 403
        }
      } 

      "with no session returns 403 (forbidden)" in {
        for{
          tokens <- get("users/"+theUser.id+"/tokens")
        } {                     
          tokens.code must be equalTo 403
        }
      }
    }        

    "/users/USERID/tokens POST" in {
      val new_user = find_or_create_user("tester")

      "with valid session" in {
        for{
          session <- post_session          
          new_token <- session.post("users/"+new_user.id+"/tokens",
            "description" -> "test token")
          tokens <- session.get("users/"+new_user.id+"/tokens")
        } {                                                                                                        
          new_token.code must be equalTo 200
          new_token.xml must \\(<description>test token</description>) 
          tokens.xml must \\(<description>test token</description>)
        }
      }

      "with valid session but no role authorization returns 403 (forbidden)" in {
		val new_user = find_or_create_user("tester")
		val new_toke = AuthToken.create.user(new_user).saveMe
		val new_token = new_toke.uniqueId.is        
		
        for{     
		  sess <- post("session", "token" -> new_token)
		  new_token <- sess.post("users/"+theUser.id+"/tokens",
		    "description" -> "test token 2")
        } {                 
          new_token.code must be equalTo 403
        }
      } 

      "with no session returns 403 (forbidden)" in {
        for{
          new_token <- post("users/"+theUser.id+"/tokens",
		    "description" -> "test token 2")
        } {                     
          new_token.code must be equalTo 403
        }
      }
    }

    "/user/messages GET" in {
      "with valid session and new messages" in {
        for{
          session <- post_session
          mess_res1 <- session.post("user/messages", "message" -> "test message") 
          timeout <- sleep(2000)
          mess_res <- session.get("user/messages")
        } {    
          mess_res.code must be equalTo 200

          // Message structure   
          (mess_res.xml \ "messages" \ "message" \ "author" \ "id").text must be equalTo (theUser.id.toString)
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("user/messages")) {
          session_res.code must be equalTo 403
        }
      }


// Should be a 304, but this response type isn't implemented in Lift yet...
      "when no new messages exist, returns 204 (no content)" in {
        for (session <- post_session;
             session_res1 <- session.get("user/messages");
             session_res <- session.get("user/messages"))
        {             
          session_res.code must be equalTo 204
        }                                                   
      }
    }

    "/user/messages?history=10 GET" in {
      "with valid session" in {
        for{
          session <- post_session
          res <- session.get("user/messages?history=10")
        } {           
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("user/messages?history=10")) {
          session_res.code must be equalTo 403
        }
      }
    }

    "/user/messages?timeout=2 GET" in {
      "with valid session" in {
        for{
          sess <- post_session   
          mess_res1 <- sess.post("user/messages", "message" -> "test message")  
          res <- sess.get("user/messages?timeout=2")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("user/messages?timeout=2")) {
          session_res.code must be equalTo 403
        }
      }

// Should be a 304, but this response type isn't implemented in Lift yet...
	  "when no new messages exist, returns 204 (no content)" in {
        for (session <- post_session;
          session_res1 <- session.get("user/messages");
          session_res <- session.get("user/messages?timeout=2"))
        {             
          session_res.code must be equalTo 204
        }                                                   
      }
    }

    "/user/messages POST" in {
      "with valid session" in {
        for{
          session <- post_session
          mess_res <- session.post("user/messages", "message" -> "test message")
        } {
          mess_res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- post("user/messages", "message" -> "test message")) {
          session_res.code must be equalTo 403
        }
      }
    }

    "/user/followees GET" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.get("user/followees")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("user/followees")) {
          session_res.code must be equalTo 403
        }
      }
    }

    "/user/followees POST" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.post("user/followees", "userId" -> "1")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (res <- post("user/followees", "userId" -> "1")) {
          res.code must be equalTo 403
        }
      }
    }

    "/user/followers GET" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.get("user/followers")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("user/followers")) {
          session_res.code must be equalTo 403
        }
      }
    }

    "/user/tracks GET" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.get("user/tracks")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("user/tracks")) {
          session_res.code must be equalTo 403
        }
      }
    }

    "/user/tracks POST" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.post("user/tracks", "track" -> ".*")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- post("user/tracks", "track" -> ".*")) {
          session_res.code must be equalTo 403
        }
      }
    }

// fragile   
    "/user/tracks/TRACKID DELETE" in {
      "with valid session" in {
        for {
          sess <- post_session
          create_track <- sess.post("user/tracks","track"->"hello")
          res <- sess.delete("user/tracks/2")
        } {           
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for {
          res <- delete("user/tracks/1")
        } {
          res.code must be equalTo 403
        }
      }
    }

    "/user/actions GET" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.get("user/actions")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (res <- get("user/actions")) {
          res.code must be equalTo 403
        }
      }
    }

    "/user/actions POST" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.post("user/actions",
            "name" -> "Test action",
            "test" -> "every 5 mins",
            "action" -> "rss:http://blog.com/feed.rss")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (res <- post("user/actions",
          "name" -> "Test action",
          "test" -> "every 5 mins",
          "action" -> "rss:http://blog.com/feed.rss")) {
          res.code must be equalTo 403
        }
      }
    }

    /*
    *    "/user/actions/ACTIONID PUT" in {
    *     "with valid session" in {
    *       for {
    *         sess <- post_session
    *         res <- sess.put("user/actions/1","enabled"->0)
    *       } {
    *         res.code must be equalTo 200
    *       }
    *     }
    *
    *     "with no session returns 403 (forbidden)" in {
    *       for(res <- post("user/actions/1","enabled"->0)) {
    *         res.code must be equalTo 403
    *       }
    *     }
    *   }
    */

// Brittle, brittle, brittle
    "/user/actions/ACTIONID DELETE" in {
      "with valid session" in {
        for {
          sess <- post_session 
          create_action <- sess.post("user/actions",
                                     "name" -> "Test action",
                                     "test" -> "every 5 mins",
                                     "action" -> "rss:http://blog.com/feed.rss")
          res <- sess.delete("user/actions/2")
        } {                     
          res.code must be equalTo 200
        }
      }
 
      "with no session returns 403 (forbidden)" in {
        for(res <- delete("user/actions/1")) {
          res.code must be equalTo 403
        }
      }
    }

// This is very ... shall we say ... brittle
    "conversations/CONVERSATIONID GET" in {
      "with valid session" in {
        for{
          sess <- post_session 
          mess_res <- sess.post("user/messages", "message"->"test")
          wait <- sleep(1000)                                
          mess_res <- sess.post("user/messages",
                                "message" -> "test_convo",
                                "replyto" -> 9)
          wait2 <- sleep(1000)                 
          res <- sess.get("conversations/9")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (res <- get("conversations/1")) {
          res.code must be equalTo 403
        }
      }

      "with an invalid conversation ID return a 404 (not found)" in {
      	for{
          sess <- post_session
          res <- sess.get("conversations/10000")
        } {
          res.code must be equalTo 404
        }
      } 
    }

    "/pools GET" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.get("pools")
        } {
          res.code must be equalTo 200      
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (res <- get("pools")) {
          res.code must be equalTo 403
        }
      }
    }

    "/pools POST" in {
      "with valid session" in {
        for{
          sess <- post_session
          res <- sess.post("pools", "poolName" -> "test_pool")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (res <- post("pools", "poolName" -> "test_pool")) {
          res.code must be equalTo 403
        }
      }
    }

    "/pools/POOLID/users POST" in {

      "with valid session" in {
        for {
          sess <- post_session
          pool_res <- sess.post("pools", "poolName" -> "test_pool2")
          res <- sess.post("pools/test_pool2/users","userId"->2)
        } {
          res.code must be equalTo 200
        }
      }

//      "a pool name that already exists returns 403 (forbidden)" in {
//        for {
//          sess <- post_session
//          pool_res <- sess.post("pools", "poolName" -> "test_pool2")
//          res <- sess.post("pools", "poolName" -> "test_pool2")
//        } {
//          sess.code must be equalTo 403
//        }
//      }

      "with no session returns 403 (forbidden)" in {
        for (res <- post("pools/1/users",
                         "realm" -> "test_realm",
                         "userId" -> 2,
                         "permission" -> "Write")) {
          res.code must be equalTo 403
        }
      }
    }
  }
}