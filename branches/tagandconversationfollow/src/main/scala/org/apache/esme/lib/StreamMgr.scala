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

package org.apache.esme.lib
import java.text.SimpleDateFormat

import net.liftweb._
import http._
import js._
import js.jquery._
import http.jquery._
import JqJsCmds._
import JsCmds._ 
import SHtml._
import JE._

import sitemap._
import Loc._

import mapper._

import util._
import common._
import Helpers._

import model._

import scala.xml._

/**
 * Manage the sitemap and related snippets for Streams
 */
object StreamMgr {
  val AnyResender = User.id.defaultValue
  val PublicPool = AccessPool.id.defaultValue

  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("base_error_not_logged_in")))

  val menuItems =
  Menu(Loc("streams", List("info_view", "streams"), S.?("base_streams_menu"), ifIsLoggedIn,
           Loc.Snippet("displayStream", displayStream),
           Loc.Snippet("streamFilters", streamFilters))) ::
  Nil

  object updateStream extends RequestVar[() => JsCmd](() => Noop)
  object resenderId extends RequestVar[Long](AnyResender)
  object filterResent_? extends RequestVar[Boolean](false)
  object poolId extends RequestVar[Long](PublicPool)
  object filterPools_? extends RequestVar[Boolean](false)  
  object spanName extends RequestVar[String]("StreamSpan")     
  object displayStreamNodes extends RequestVar[NodeSeq](Text(""))  
  
  def updateSpan(): JsCmd = SetHtml(spanName, doRender(displayStreamNodes))    
  
  // bind the dynamic content to the incoming nodeseq
  def doRender(in: NodeSeq): NodeSeq = {   
    // get the current user
    val user = User.currentUser
  
    val resentQuery = 
    if (filterResent_?.is == false) Nil
    else {
      val queryParam = resenderId.is match {
        case AnyResender => NotBy(Mailbox.resentBy, Empty)
        case id => By(Mailbox.resentBy, id)
      }
      List(In(Message.id,Mailbox.message,By(Mailbox.user, user), queryParam))
    }
    
    val poolsQuery = 
    if (filterPools_?.is == false) Nil
    else List(poolId.is match {
      case PublicPool => By(Message.pool, Empty)
      case id => By(Message.pool, id)
    })
    
    val query = poolsQuery :::
                resentQuery :::
                List[QueryParam[Message]](OrderBy(Message.id, Descending), MaxRows(10)) 
       
    def profileImage(u: User): NodeSeq = {
        var imageUrl = u.imageUrl.toString
        if (imageUrl.length > 0) 
          <img width="30px" src={imageUrl}/>
        else
           <img width="30px" src="/images/avatar.jpg"/>
     }
         
    //XXX copy from lib.UserMgr
    def nicknameWithProfileLink(u: User): NodeSeq = {
  		  <a href={"/user/" + urlEncode(u.nickname.is)}>{u.niceName}</a>
    	}
    	
    val dateFormatter = new SimpleDateFormat("yyyy/MM/dd HH:mm")
        
      
    Message.findAll(query: _*) match {
      case Nil => NodeSeq.Empty
      case xs => bind("disp", in,
                      "item" -> 
                      (lst => xs.flatMap(i => bind("item", lst,
                                                   "author" -> i.author.obj.map(nicknameWithProfileLink).openOr(Text("")),
                                                   "imageUrl" -> i.author.obj.map(profileImage).openOr(Text("")),
                                                   "text" -> i.getBody,
                                                   "date" -> dateFormatter.format(i.getWhen)
              ))))
    }
  }  
  

  def displayStream(in: NodeSeq): NodeSeq = {    
      
    updateStream.set(updateSpan)    
    displayStreamNodes.set(in)                               
  
    doRender(in)
  }

  def streamFilters(in: NodeSeq): NodeSeq = {   
  
    import org.apache.esme.model.AccessPool
    import net.liftweb.common.Empty              
    val resenderInput = "resender_input"
    val poolInput = "pool_input"
    val filterResentInput = "filter_resent_input"
    val filterPoolsInput = "filter_pools_input"
    val user = User.currentUser
    
    var resender = AnyResender
    var pool = PublicPool
    var filterResent = false
    var filterPools = false
    
    val following = (AnyResender.toString, S.?("base_streams_resend_any")) ::
    (user match {
      case Full(u) => u.following.map(u => (u.id.is.toString, u.nickname.is) )
      case _ => Nil
    }) 
    
    val pools = (PublicPool.toString, S.?("base_streams_pool_default")) ::
    (user match {
      case Full(u)=> Privilege.findViewablePools(u.id).map(
        p => (p.toString, AccessPool.find(p).get.getName)).toList
      case _ => Nil
    })
    
    def redisplay(): JsCmd = {
      resenderId.set(resender)
      poolId.set(pool)
      filterResent_?.set(filterResent)
      filterPools_?.set(filterPools)
      updateStream()
    }

    /*
    bind("main", in,
         "resent" -> ajaxSelect(following,
                                Empty,
                                u => {resender = u.toLong
                                      redisplay()},
                                "id" -> resenderInput),
         "pools" -> ajaxSelect(pools,
                               Empty,
                               p => {pool = p.toLong
                                     redisplay()},
                               "id" -> poolInput),
         "filterResent" -> ajaxCheckbox(false,
                                        r_? => {filterResent = r_?
                                                 redisplay()},
                                        "id" -> filterResentInput),
         "filterPools" -> ajaxCheckbox(false,
                                       p_? => {filterPools = p_?
                                       redisplay()},
                                       "id" -> filterPoolsInput)
    )
    */

    import org.apache.esme.liftwebext.SHtml._ 
    bind("main", in,
         "resent" -> ajaxSortedSelect(following,
                                true,
                                Empty,
                                u => {resender = u.toLong
                                      redisplay()},
                                "id" -> resenderInput),
         "pools" -> ajaxSortedSelect(pools,
                               true,
                               Empty,
                               p => {pool = p.toLong
                                     redisplay()},
                               "id" -> poolInput),
         "filterResent" -> ajaxCheckbox(false,
                                        r_? => {filterResent = r_?
                                                 redisplay()},
                                        "id" -> filterResentInput),
         "filterPools" -> ajaxCheckbox(false,
                                       p_? => {filterPools = p_?
                                       redisplay()},
                                       "id" -> filterPoolsInput)
    )
  }
}
