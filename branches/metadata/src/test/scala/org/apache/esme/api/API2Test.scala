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
import net.liftweb.mapper.{By}
import org.specs.matcher._
import Helpers._
import org.apache.esme._
import model._
import net.liftweb.http._
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework, TestResponse}

import _root_.junit.framework.AssertionFailedError

class Api2SpecsAsTest extends JUnit3(Api2Specs)
object Api2SpecsRunner extends ConsoleRunner(Api2Specs)

object Api2Specs extends Specification with TestKit {
  JettyTestServer.start

  val baseUrl = JettyTestServer.urlFor("/api2/")

  // Note "api_test" user is special. It has been set up with the integration-admin
  // role in the test.default.props file.

  val theUser = find_or_create_user("api_test")
  val token = find_or_create_token(theUser)

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

  def find_or_create_token(tokenUser: User): String = {
    val token: Box[AuthToken] = AuthToken.find(By(AuthToken.user,tokenUser))

    if(token.isDefined)
      token.open_!.uniqueId.is
    else {
      val toke = AuthToken.create.user(tokenUser).saveMe
      toke.uniqueId.is
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
          xml <- session.xml
        } {
          (xml \ "session" \ "user" \ "id").text must be equalTo (theUser.id.toString)
          (xml \ "session" \ "user" \ "nickname").text must be equalTo (theUser.niceName)
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
          xml <- session_response.xml
        } {
          session_response.code must be equalTo 200
          (xml \ "session" \ "user" \ "id").text must be equalTo (theUser.id.toString)
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
          //session_response <- session.get[Response]("session")
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
          xml <- all_users.xml
        } {
          added_user.code must be equalTo 200
          (xml \ "users") must \\(<nickname>test_user5</nickname>)
        }
      }

      "with a valid session but no role authorization returns 403 (forbidden)" in {
        val new_user = find_or_create_user("tester2")
        val new_token = find_or_create_token(new_user)

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
      val new_user = find_or_create_user("tester3")
      val new_token = find_or_create_token(new_user)

      "with valid session" in {
        for{
          session <- post_session
          tokens <- session.get("users/"+new_user.id+"/tokens")
        } {
          tokens.code must be equalTo 200
          tokens.xml.open_! must \\(<id>{new_token}</id>)
        }
      }

      "with valid session but no role authorization returns 403 (forbidden)" in {
        val new_user = find_or_create_user("tester4")
        val new_token = find_or_create_token(new_user)

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
      val new_user = find_or_create_user("tester5")

      "with valid session" in {
        for{
          session <- post_session
          new_token <- session.post("users/"+new_user.id+"/tokens",
            "description" -> "test token")
          tokens <- session.get("users/"+new_user.id+"/tokens")
        } {
          new_token.code must be equalTo 200
          new_token.xml.open_! must \\(<description>test token</description>)
          tokens.xml.open_! must \\(<description>test token</description>)
        }
      }

      "with valid session but no role authorization returns 403 (forbidden)" in {
        val new_user = find_or_create_user("tester6")
        val new_token = find_or_create_token(new_user)

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
          (mess_res.xml.open_! \ "messages" \ "message" \ "author" \ "id").text must be equalTo (theUser.id.toString)
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
          res <- session.post("pools", "poolName" -> "test_pool1")
          mess_res <- session.post("user/messages",
            "message" -> "test POST message",
            "tags" -> "test,tag",
            "pool" -> "test_pool1")
          timeout <- sleep(1000)
          all_msgs <- session.get("user/messages?timeout=2")
        } {
          mess_res.code must be equalTo 200
          (mess_res.xml.open_! \ "message") must \\ (<body>test POST message</body>)
          (mess_res.xml.open_! \ "message") must \\ (<tags><tag>Test</tag><tag>Tag</tag></tags>)
          (all_msgs.xml.open_! \ "messages") must \\(<body>test POST message</body>)
          (all_msgs.xml.open_! \ "messages") must \\(<tags><tag>Test</tag><tag>Tag</tag></tags>)
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- post("user/messages", "message" -> "test message")) {
          session_res.code must be equalTo 403
        }
      }      
      
      "with XML metadata" in {
        for{
          session <- post_session       
          mess_res <- session.post("user/messages",
            "message" -> "test POST message",
            "metadata" -> "<outer><meta><metameta>Hello</metameta></meta><onlymeta>Meta</onlymeta></outer>")              
        } {
          mess_res.code must be equalTo 200            
          (mess_res.xml.open_! \ "message") must \\ (<body>test POST message</body>)    
          (mess_res.xml.open_! \\ "metadata") must be equalTo <metadata><outer><meta><metameta>Hello</metameta></meta><onlymeta>Meta</onlymeta></outer></metadata>         
        }
      }  
      
      "with JSON metadata" in {
        for{
          session <- post_session       
          mess_res <- session.post("user/messages",
            "message" -> "test POST message",
            "metadata" -> """"meta":[{"place":{"place_type":"city","region":"CA+"}},{"song":{"artist":"Prince","songtitle":"Never+Let+Me+Down"}}]""")              
        } {
          mess_res.code must be equalTo 200                  
          (mess_res.xml.open_! \ "message") must \\ (<body>test POST message</body>)    
          (mess_res.xml.open_! \\ "metadata") must be equalTo <metadata>{""""meta":[{"place":{"place_type":"city","region":"CA+"}},{"song":{"artist":"Prince","songtitle":"Never+Let+Me+Down"}}]"""}</metadata>         
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
          ( res.xml.open_! \\ "message" ).last must \\(<conversation>9</conversation>)
          ( res.xml.open_! \\ "message" ).last must \\(<replyto>9</replyto>)
          ( res.xml.open_! \\ "message" ).first must \\(<conversation>9</conversation>)
          ( res.xml.open_! \\ "message" ).first must \\(<replyto></replyto>)
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
          res <- sess.post("pools", "poolName" -> "test_pool2")
        } {
          res.code must be equalTo 200
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (res <- post("pools", "poolName" -> "failed_test_pool")) {
          res.code must be equalTo 403
        }
      }
    }

    "/pools/POOLID/users POST" in {

      "with valid session" in {
        for {
          sess <- post_session
          pool_res <- sess.post("pools", "poolName" -> "test_pool3")
          res <- sess.post("pools/3/users","userId"->2,"permission"->"Write")
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

    "/pools/POOLID/messages GET" in {
      "with valid session and new messages" in {
        for{
          sess <- post_session
          pool_res <- sess.post("pools", "poolName" -> "test_pool4")
          init <- sess.get("pools/4/messages")
          timeout <- sleep(2000)
          mess_res1 <- sess.post("user/messages",
            "message" -> "test message for pool delta",
            "pool" -> "test_pool4")
          timeout <- sleep(2000)
          mess_res <- sess.get("pools/4/messages")
        } {                     
          mess_res.code must be equalTo 200

          // Message structure
          (mess_res.xml.open_! \ "messages") must \\(<id>{theUser.id.toString}</id>)
          (mess_res.xml.open_! \ "messages") must \\(<body>test message for pool delta</body>)
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("pools/1/messages")) {
          session_res.code must be equalTo 403
        }
      }

      // Should be a 304, but this response type isn't implemented in Lift yet...
      "when no new messages exist, returns 204 (no content)" in {
        for (session <- post_session;
             session_res1 <- session.get("pools/1/messages");
             session_res <- session.get("pools/1/messages"))
        {
          session_res.code must be equalTo 204
        }
      }
    }

    "/pools/POOLID/messages?history=10 GET" in {
      "with valid session" in {
        for{
          sess <- post_session
          pool_res <- sess.post("pools", "poolName" -> "test_pool5")
          mess_res <- sess.post("user/messages",
            "message" -> "test message for pool history",
            "pool" -> "test_pool5")
          timeout <- sleep(2000)
          res <- sess.get("pools/5/messages?history=10")
        } {
          res.code must be equalTo 200
          (res.xml.open_! \ "messages") must \\(<id>{theUser.id.toString}</id>)
          (res.xml.open_! \ "messages") must \\(<body>test message for pool history</body>)
        }
      }

      "with tag restrictions" in {
        for{
          sess <- post_session
          pool_res <- sess.post("pools", "poolName" -> "test_pool6")
          mess_res <- sess.post("user/messages",
            "message" -> "test message for pool #history",
            "pool" -> "test_pool6")
          mess_res <- sess.post("user/messages",
            "message" -> "test message for pool history with tags test, tag",
            "pool" -> "test_pool6",
            "tags" -> "test, tag")
          mess_res <- sess.post("user/messages",
            "message" -> "test message for pool history with tag test",
            "pool" -> "test_pool6",
            "tags" -> "test,crazycrazy")
          mess_res <- sess.post("user/messages",
            "message" -> "test message for pool history with tag tag",
            "pool" -> "test_pool6",
            "tags" -> "tag")
          wait <- sleep(2000)
          res1 <- sess.get("pools/6/messages?history=10&filter_tags=test")      
          res2 <- sess.get("pools/6/messages?history=10&filter_tags=test,tag")
        } {                               
          res1.code must be equalTo 200   
          res2.code must be equalTo 200       
          (res1.xml.open_! \ "messages") must \\(<id>{theUser.id.toString}</id>)
          (res1.xml.open_! \ "messages") must \\(<body>test message for pool history with tag test</body>)
          (res1.xml.open_! \ "messages") must \\(<body>test message for pool history with tags test, tag</body>) 
          (res2.xml.open_! \ "messages") must \\(<body>test message for pool history with tags test, tag</body>)
          (res2.xml.open_! \ "messages").length must be equalTo 1
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("pools/1/messages?history=10")) {
          session_res.code must be equalTo 403
        }
      }
    }

    "/pools/POOLID/messages?timeout=2 GET" in {
      "with valid session" in {
        for{
          sess <- post_session
          pool_res <- sess.post("pools", "poolName" -> "test_pool6")
          init <- sess.get("pools/6/messages")
          timeout <- sleep(2000)
          mess_res <- sess.post("user/messages",
            "message" -> "test message for pool timeout",
            "pool" -> "test_pool6")
          timeout <- sleep(2000)
          res <- sess.get("pools/6/messages?timeout=2")
        } {
          res.code must be equalTo 200
        }
      }

      "with valid session and new messages but no pool authorization returns 403 (forbidden)" in {
        val new_user = find_or_create_user("tester6")
        val new_token = find_or_create_token(new_user)

        for{
          sess <- post_session
          sessNoAuth <- post("session", "token" -> new_token)
          pool_res <- sess.post("pools", "poolName" -> "test_pool7")
          init <- sessNoAuth.get("pools/7/messages")
          timeout <- sleep(2000)
          mess_res1 <- sess.post("user/messages",
            "message" -> "test message for pool delta",
            "pool" -> "test_pool7")
          timeout <- sleep(2000)
          mess_res <- sessNoAuth.get("pools/7/messages")
        } {
          mess_res.code must be equalTo 403
          init.code must be equalTo 403
        }
      }

      "with no session returns 403 (forbidden)" in {
        for (session_res <- get("pools/1/messages?timeout=2")) {
          session_res.code must be equalTo 403
        }
      }

    // Should be a 304, but this response type isn't implemented in Lift yet...
      "when no new messages exist, returns 204 (no content)" in {
        for (session <- post_session;
          session_res1 <- session.get("pools/1/messages");
          session_res <- session.get("pools/1/messages?timeout=2"))
        {
          session_res.code must be equalTo 204
        }
      }
    }
  }
}
