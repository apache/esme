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
import org.apache.esme.actor.Distributor

import scala.xml._

import java.util.Date
import java.text.{DateFormat,SimpleDateFormat}

/**
 * Manage the sitemap and related snippets for Access Pools
 */
object AccessPoolMgr {
  val logger: Logger = Logger("org.apache.esme.lib.AccessPoolMgr")
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("base_error_not_logged_in")))

  val menuItems =
  Menu(Loc("accessPools", List("pools_view", "index"), S.?("base_pool_menu"), ifIsLoggedIn,
           Loc.Snippet("addPool", addPool),
           Loc.Snippet("editPool", editPool),
           Loc.Snippet("poolUsers", displayPoolUsers),
  		   Loc.Snippet("poolDetail", displayPoolDetail)//regist snippet for pool detail display
  		)) ::
  Nil

  object updatePool extends RequestVar[() => JsCmd](() => Noop)
  
  //update pool detail response 
  object updatePoolDetail extends RequestVar[() => JsCmd](() => Noop)
  
  
  object poolId extends RequestVar[Long](0)


  /*
  * Function for adding pools
  *
  */
  def addPool(in: NodeSeq): NodeSeq = {
  
    val theInput = "new_pool"
    val user = User.currentUser 
    
      
    def addNewPool(name: String) = {
      name.trim match {
        case x if x.length < 3 => DisplayMessage("messages", <b>{S.?("base_pool_error_name_short")}</b>,  3 seconds, 3 seconds)
        case x => {
          val pool = AccessPool.create.realm(AccessPool.Native).setName(name)
          pool match {
            case Failure(_,_,_) => DisplayMessage("messages", <b>{S.?("base_pool_msg_duplicate_name_pool",name)}</b>,  3 seconds, 2 seconds)
            case Full(p: AccessPool) => val privilegeSaved =
              Privilege.create.pool(p.saveMe).user(user).permission(Permission.Admin).save
              if(privilegeSaved && user.isDefined) {
                Distributor ! Distributor.AllowUserInPool(user.get.id.is, p.id.is)         
                logger.info("ACCESS: " + S.?("base_pool_msg_new_pool",name)) 
                SetValById(theInput, "")  &
                DisplayMessage("messages", <b>{S.?("base_pool_msg_new_pool",name)}</b>,  3 seconds, 2 seconds) 
              } else
                DisplayMessage("messages", <b>{S.?("base_pool_msg_no_permission")}</b>,  3 seconds, 2 seconds)
            case _ => S.error(S.?("base_error_general"))
          }
        }
      }

    }

    bind("add", in,
         "poolName" -%> text("", addNewPool, "id" -> theInput)
    )
    
  }

  /*
  * Function for editing pools
  *
  */
  def editPool(in: NodeSeq): NodeSeq = {

    val redisplayPool = updatePool.is
    
    // redisplay pool detail
    val redisplayPoolDetail = updatePoolDetail.is
    
    // redisplay pool users and pool detail
    def redisplay(): JsCmd = {
      redisplayPoolDetail() & redisplayPool() 
    }
    var pool = ""
    var username = ""
    val editPoolName = "edit_pool"
    val editUsername = "edit_username"
    val editPermission = "edit_permission"
    val adminUser = User.currentUser
    
    val adminPools = ("0", S.?("base_pool_msg_choose_pool")) ::
    (adminUser match {
      case Full(u)=> Privilege.findViewablePools(u.id).map(
        p => (p.toString, AccessPool.find(p).get.getName)).toList
      case _ => Nil
    })
      
    val permissions = Permission.map(perm => (perm.id.toString, perm.toString)).collect
    
    
      /*
       * Function for adding a user to a pool
       *
       */
    def addPoolUser(permission: String): JsCmd = {
      val r: Box[Boolean] = 
      for (admin <- adminUser;
           p <- AccessPool.find(pool) ?~ DisplayMessage("messages", <b>{S.?("base_pool_err_pool_not_found")}</b>,  3 seconds, 2 seconds);
           user <- User.findFromWeb(username) ?~ DisplayMessage("messages", <b>{S.?("base_pool_err_pool_not_found")}</b>,  3 seconds, 2 seconds)
      ) yield if(Privilege.hasPermission(admin.id.is, p.id.is, Permission.Admin)) {
        val result = try {
          Privilege.create.user(user).pool(p).permission(Permission(permission.toInt)).save
        } catch {
          case _: Exception => false
        }
        if (result) {
        	Distributor ! Distributor.AllowUserInPool(user.id.is, p.id.is)
        	DisplayMessage("messages", <b>{S.?("base_pool_msg_permission_set")}</b>,  3 seconds, 2 seconds)
       }
        result
      } else false // "User has no permission to administer pool"
      r match {
        case Failure(m,_,_) => S.error(m)
        case Full(true) => S.notice(S.?("base_pool_msg_permission_set"))
        case _ => S.error(S.?("base_error_general"))
      }
      
      poolId.set(pool.toLong)
      
      //we needn't redisplay pool detail when add a new user
      redisplayPool() & SetValById(editUsername, "")
    }

    bind("edit", in,
         "pool" -%> ajaxSelect(adminPools, Empty, p => {pool = p;
                                                       poolId.set(p.toLong);
                                                       redisplay() //redisplay pooluser and pool detail
                                                       },
                                                 "id" -> editPoolName),
         "username" -%> text(username, username = _, "id" -> editUsername),
         "permission" -%> select(permissions, Empty, addPoolUser, "id" -> editPermission)
    )
    
  }
  
  
  def displayPoolDetail(in: NodeSeq): NodeSeq = {
    // get the span name to update
    val spanName = S.attr("the_pool_id") openOr "PoolDetailSpan"
    
    //XXX display date, should we have a common dateFormat?
    val dateFormat = new SimpleDateFormat("yyyy/MM/dd")
    def getDateHtml(date: Date) : Text = date match {
     case null => Text(S.?("base_pool_ui_empty_date"))
     case d => Text(dateFormat.format(d))
   }
    
    def displayUserName(uid: Long): NodeSeq = {
      User.find(uid) match {
        case Full(user) => <span>{user.nickname}</span>
        case _ => NodeSeq.Empty
      }
    }
    
    def doRender(): NodeSeq = 
       AccessPool.find(By(AccessPool.id, poolId.is)) match {
        case Full(ap) => bind(
        "pool", in,
        "name" -> ap.getName,     
        "creator" -> displayUserName(ap.creator),
        "createdDate" -> getDateHtml(ap.createdDate),
        "modifier" -> displayUserName(ap.modifier),
        "lastModifyDate" -> getDateHtml(ap.lastModifyDate))
      case _ => NodeSeq.Empty
      }
    
    def updateSpan(): JsCmd = SetHtml(spanName, doRender())
    updatePoolDetail.set(updateSpan)
    doRender
  }
  
  def displayPoolUsers(in: NodeSeq): NodeSeq = {
    // get the span name to update
    val spanName = S.attr("the_id") openOr "PoolSpan"
    // get the current user
    val user = User.currentUser

      // Distributor ! Distributor.RefreshUser(userId)
    def doRender(): NodeSeq = {
    val accessPool = AccessPool.find(By(AccessPool.id, poolId.is))  
    Privilege.findAll(By(Privilege.pool, poolId.is), NotBy(Privilege.permission, Permission.Denied)) match {
      case Nil => NodeSeq.Empty
      case xs => bind("pool", in,
                      "user" -> 
                      (lst => xs.flatMap(i => bind("user", lst,
                                                   "name" -> User.find(i.user).map(
                                                             _.nickname.is).getOrElse(""),
                                                   "privilege" -> i.permission.is.toString
                      ))))
    }
    }

    def updateSpan(): JsCmd = SetHtml(spanName, doRender())

    updatePool.set(updateSpan)
    doRender
 }
}
