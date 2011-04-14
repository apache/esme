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

package org.apache.esme.comet   

import net.liftweb.common._  
import net.liftweb.http._         
import net.liftweb.util.Helpers.TimeSpan 

import org.apache.esme._
import actor.{Distributor,UserActor}
import model._                
import net.liftweb.http.js.jquery.JqJsCmds.{PrependHtml}  

class SearchTimeline extends Timeline {   

  val jsId = "search_timeline_messages"    
  
  override def localSetup() {
    super.localSetup()
                                                            
    for( user <- User.currentUser;
         n <- name ) {                
      messages = Message.search(n, user.following, 50).map( m => 
        (m.id.is, NoReason, false))
    }
  }         
  
  override def lifespan = Full(TimeSpan(100000))
  
}