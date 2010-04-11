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

package org.apache.esme.external

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import org.specs.matcher._                             
import org.apache.esme.model.{Action,User,Mailbox}                  
                                                 
import _root_.junit.framework.AssertionFailedError 
                 
import net.liftweb.util._
import net.liftweb.common._         
import Helpers._                                 
import net.liftweb.http._ 
import net.liftweb.http.testing.{TestKit, HttpResponse, TestFramework}    

class PubSubHubbubSpecsAsTest extends JUnit3(PubSubHubbubSpecs)
object PubSubHubbubSpecsRunner extends ConsoleRunner(PubSubHubbubSpecs)

object PubSubHubbubSpecs extends Specification with TestKit { 
  
// Consolidate into test helpers
  def find_or_create_user(userName: String): User = {
    val users = User.findByNickname(userName)

    if(users.length > 0)
      users.head
    else { 
      val session = new LiftSession(Helpers.randomString(20), "", Empty)                          
      S.initIfUninitted(session) {User.createAndPopulate.nickname(userName).saveMe}
    }
  }      

// Consolidate into test helpers
  def sleep(wait: Long): Box[Boolean] = {
    Thread.sleep(wait)
    Full(true)
  }

  JettyTestServer.start

  val baseUrl = JettyTestServer.urlFor("/push/")

  "PubSubHubbubReceiver" should {
    "always return 200 OK response after receiving a callback" in {
      for{
        session <- post("blankcallback")
        session2 <- post("blahblahblahblah")
        session3 <- post("")
      } {
        session.code must be equalTo 200
        session2.code must be equalTo 200
        session3.code must be equalTo 200
      }
    }

    "with callback receiver, create new message" in {     
      val user = find_or_create_user("push_test")  
      val action = Action.create
                         .user(user)
                         .name("Push test")
                         .setTest("every 5 mins").open_!      
                         .setAction("rss:http://blog.com/feed.rss").open_!
      PubSubHubbubReceiver.addReceivingAction(action)  
                
      for{
        session <- post("testcallbackurl1")
        wait <- sleep(1000)
      } {
        val message = Mailbox.mostRecentMessagesFor(user.id.is,1).head 

        message match {
          case (mes,_,_) => mes.body must be equalTo "Some Text"
        }                      

        session.code must be equalTo 200
      }
    }

    "without callback receiver, return 200 but don't create message" in {

    }          
  }
}