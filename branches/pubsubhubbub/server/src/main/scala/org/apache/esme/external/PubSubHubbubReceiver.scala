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
                            
import net.liftweb.common.{Full,Box,Empty}
import net.liftweb.http.{Req,PostRequest,LiftRules,OkResponse,LiftResponse}   

import org.apache.esme.model.Action    

import org.apache.esme.actor.Distributor    

import scala.collection.mutable.HashMap

object PubSubHubbubReceiver {  

  private val receivingActions = new HashMap[String,Box[Action]]

  def dispatch: LiftRules.DispatchPF = {
    case Req("push" :: cbCode :: Nil, _, PostRequest) => () => handleCallBack(cbCode)
  }           

  def addReceivingAction(who:Action) {
// Replace with real code to register callback URL based on action
    receivingActions += Tuple2("blahblahblahblah",Full(who))    
    receivingActions += Tuple2("testcallbackurl1",Full(who))
  }                                                                 

  protected def handleCallBack(cbCode:String): Box[LiftResponse] = { 
                      
    // See if the callback code is registered
    val action = receivingActions.get(cbCode)

    for(a1 <- action; a <- a1) yield {          
      val user = a.user
      Distributor ! Distributor.UserCreatedMessage(
        user,
        "Some Text",
        List("tags"),
        1,
        Empty,
        "PubSubHubbub baby",
        Empty,
        None)
    }
    
// According to the spec, 200-response indicates only successful receipt, not processing
    Full(OkResponse())  
  }

}