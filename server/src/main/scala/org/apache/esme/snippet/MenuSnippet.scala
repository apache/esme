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

package org.apache.esme.snippet

import org.apache.esme._        
import net.liftweb._
import http._     
import util._  
import scala.xml.NodeSeq

class MenuSnippet extends DispatchSnippet {
  def dispatch: DispatchIt = Map(
    "menu" -> menu _
  )                 
  
  def menu(n:NodeSeq):NodeSeq = {
    
    val itemHome: NodeSeq = <li><lift:Menu.item donthide="true" name="Home" a:class="tipelement" a:title="This is the your timeline."/></li> 
    
    val itemProfile: NodeSeq = <li><lift:Menu.item donthide="true" name="profile" a:class="tipelement" a:title="You can change your profile."/></li> 
    
    val itemTracks: NodeSeq = 
      if(Props.get("esme.enable_tracks").openOr("true") == "true") {
        <li><lift:Menu.item donthide="true" name="trackMgt" a:class="tipelement" a:title="You can track words."/></li> 
      } else {
        List()
      }
    
    val itemActions: NodeSeq = 
      if(Props.get("esme.enable_actions").openOr("true") == "true") {
        <li><lift:Menu.item donthide="true" name="actionMgt" a:class="tipelement" a:title="You can create actions to fire when certain events occur"/></li>  
      } else {
        List()
      }
    
    val itemTokens: NodeSeq = 
      if(Props.get("esme.enable_tokens").openOr("true") == "true") {
        <li><lift:Menu.item donthide="true" name="authToken" a:class="tipelement" a:title="You can create tokens to use when accessing ESME via other clients."/></li>
      } else {
        List()
      }
        
    val itemPools: NodeSeq = <li><lift:Menu.item donthide="true" name="accessPools" a:class="tipelement" a:title="You can create access pools to restrict access top certain messages."/></li>    
    
    val itemContacts: NodeSeq = <li><lift:Menu.item donthide="true" name="contacts" a:class="tipelement" a:title="You can manage your followers and those you follow."/></li>     
    
    val itemList = itemHome ++ itemProfile ++ itemTracks ++ itemActions ++ itemTokens ++ itemPools ++ itemContacts 
    
    <ul class="main-links">{itemList}</ul>
  }
}