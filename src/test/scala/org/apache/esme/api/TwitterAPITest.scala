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

import scala.xml._
import scala.actors.Actor
import scala.actors.TIMEOUT

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.actor.LiftActor
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper.{By}
import org.specs.matcher._
import Helpers._
import org.apache.esme._
import model._
import org.apache.esme.actor.Distributor
import org.apache.esme.actor.UserActor.MessageReceived
import org.apache.esme.actor.Distributor.Listen
import net.liftweb.http._
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework, TestResponse, Response}

import _root_.junit.framework.AssertionFailedError

class TwitterAPISpecsAsTest extends JUnit3(TwitterAPISpecs)
object TwitterAPISpecsRunner extends ConsoleRunner(TwitterAPISpecs)

object TwitterAPISpecs extends Specification with TestKit {
  JettyTestServer.start

  val baseUrl = JettyTestServer.urlFor(TwitterAPI.ApiPath.mkString("/", "/", ""))

  val userName = "twitter_user"
  val theUser = find_or_create_user(userName)
  val token = find_or_create_token(theUser)

  val followerName = "twitter_follower"
  val followerUser = find_or_create_user(followerName)
  val followerToken = find_or_create_token(followerUser)

  implicit val reportError = new ReportFailure {
    def fail(msg: String): Nothing = TwitterAPISpecs.this.fail(msg)
  }

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
      val token = AuthToken.create.user(tokenUser).saveMe
      token.uniqueId.is
    }
  }
  
  override def theHttpClient = {
    val theClient = buildBasicAuthClient(userName, token)
    theClient.getParams.setAuthenticationPreemptive(true)
    theClient
  }
  
  val noAuthClient = buildNoAuthClient
  
  val followerClient = {
    val theClient = buildBasicAuthClient(followerName, followerToken)
    theClient.getParams.setAuthenticationPreemptive(true)
    theClient
  }
  
  class BridgeActor(receiver: Actor) extends LiftActor {
    protected def messageHandler = {
      case nm @ MessageReceived(_, _) => receiver ! nm
    }
  }
  
  case object Wait
  
  class ConductorActor extends Actor {
    def act {
      react {
        case Wait => reply {
          receive {
            case MessageReceived(msg, reason) => msg
          }
        }
      }
    }
  }
  
  // register an actor to be notified when a new message is received
  val conductor = new ConductorActor
  conductor.start
  val liftActor = new BridgeActor(conductor)
  Distributor ! Listen(theUser.id.is, liftActor)
  Distributor ! Listen(followerUser.id.is, liftActor)
  
  trait XmlResponse {
    self: TestResponse =>
    def xmlMatch(f: Elem => Unit)(implicit errorFunc: ReportFailure): TestResponse = {
      if (this.asInstanceOf[SelfType].code != 200) errorFunc.fail("Response status is not 200!")
      xml match {
        case Full(xml) => f(xml); this
        case _ => errorFunc.fail("Response contains no XML!")
      }
    }
      
    def \\(node: Node): TestResponse = xmlMatch(_ must XmlBaseMatchers.\\(node))
    def !\\(node: Node): TestResponse = xmlMatch(_ must not(XmlBaseMatchers.\\(node)))
    def \(node: Node): TestResponse = xmlMatch(_ must XmlBaseMatchers.\(node))
    def !\(node: Node): TestResponse = xmlMatch(_ must not(XmlBaseMatchers.\(node)))
    def \\(label: String): TestResponse = xmlMatch(_ must XmlBaseMatchers.\\(label))
    def !\\(label: String): TestResponse = xmlMatch(_ must not(XmlBaseMatchers.\\(label)))
    def \(label: String): TestResponse = xmlMatch(_ must XmlBaseMatchers.\(label))
    def !\(label: String): TestResponse = xmlMatch(_ must not(XmlBaseMatchers.\(label)))
  }
  
  implicit def testResponse2XmlResponse(response: TestResponse): XmlResponse = {
    val r = response.asInstanceOf[response.SelfType]
    new response.SelfType(r.baseUrl, r.code, r.msg, r.headers, r.body, r.theHttpClient) with XmlResponse
  }
  
  "Twitter API" should {
  
    "post a message" in {
      post("/statuses/update.xml", "status" -> "test_msg1") \\(<text>test_msg1</text>)
      // wait till the message appears in the timeline
      // or fail after 5 seconds
      val msgReceived = conductor !? (5000L, Wait)
      if (msgReceived.isEmpty) fail("no message received")
    }
    
    "fail to post a message with no status parameter" in {
      post("/statuses/update.xml") \\("error")
    }
    
    "fail to post a message when no user credentials supplied" in {
      post("/statuses/update.xml", noAuthClient, Nil, "status" -> "test_msg1") \\("error")
    }
    
    "show message in user, home and public timelines" in {
      get("/statuses/public_timeline.xml") \\(<text>test_msg1</text>)
      get("/statuses/user_timeline.xml") \\(<text>test_msg1</text>)
      get("/statuses/home_timeline.xml") \\(<text>test_msg1</text>)
    }
    
    "not show message in user and home timelines when not authenticated" in {
      get("/statuses/user_timeline.xml", noAuthClient, Nil) \\("error")
      get("/statuses/home_timeline.xml", noAuthClient, Nil) \\("error")
    }

    "create a friendship" in {
      post("/friendships/create/" + userName + ".xml", followerClient, Nil) \\(<screen_name>twitter_user</screen_name>)
    }
    
    "let user see new follower in followers" in {
      get("/statuses/followers.xml") \\(<screen_name>twitter_follower</screen_name>)
    }
    
    "not let follower see user in followers" in {
      get("/statuses/followers.xml", followerClient, Nil) !\\(<screen_name>twitter_user</screen_name>)
    }
    
    "not let user see new follower in friends" in {
      get("/statuses/friends.xml") !\\(<screen_name>twitter_follower</screen_name>)
    }
    
    "let follower see user in friends" in {
      get("/statuses/friends.xml", followerClient, Nil) \\(<screen_name>twitter_user</screen_name>)
    }
    
    "let follower see user's message in home timeline" in {
      post("/statuses/update.xml", "status" -> "user_msg") \\(<text>user_msg</text>)
      
      // wait till the message appears in the timeline
      // or fail after 5 seconds
      val msgReceived = conductor !? (5000L, Wait)
      if (msgReceived.isEmpty) fail("no message received")
      
      get("/statuses/home_timeline.xml", followerClient, Nil) \\(<text>user_msg</text>)
    }
    
    "not let user see follower's message in home timeline" in {
      post("/statuses/update.xml", followerClient, Nil, "status" -> "follower_msg") \\(<text>follower_msg</text>)

      get("/statuses/home_timeline.xml") !\\(<text>follower_msg</text>)
    }
    
    "let user see follower's message in public timeline" in {
      get("/statuses/public_timeline.xml") \\(<text>follower_msg</text>)
    }
    
    "destroy a friendship" in {
      post("/friendships/destroy/" + userName + ".xml", followerClient, Nil) \\(<screen_name>twitter_user</screen_name>)
    }
    
  }
}

