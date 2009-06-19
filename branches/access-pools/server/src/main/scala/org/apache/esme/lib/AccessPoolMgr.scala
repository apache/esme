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

package org.apache.esme.lib

import net.liftweb._
import http._
import SHtml._
import js._
import JsCmds._
import JE._

import sitemap._
import Loc._

import mapper._

import util._
import Helpers._

import model._
import org.apache.esme.actor.Distributor

import scala.xml._

/**
 * Manage the sitemap and related snippets for Access Pools
 */
object AccessPoolMgr {
  def loggedIn_? = User.loggedIn_?

  val ifIsLoggedIn = If(loggedIn_? _, strFuncToFailMsg(() => S.?("You must be logged in")))

  val menuItems =
  Menu(Loc("accessPools", List("pools_view", "index"), "Manage Access Pools", ifIsLoggedIn,
           Loc.Snippet("addPool", addPool),
           Loc.Snippet("editPool", editPool),
           Loc.Snippet("poolUsers", displayPoolUsers))) ::
  Nil

  object updatePool extends RequestVar[() => JsCmd](() => Noop)
  object poolId extends RequestVar[Long](0)

  def addPool(in: NodeSeq): NodeSeq = {
    val theInput = "new_pool"
    val user = User.currentUser
    
    def addNewPool(name: String) = {
      name.trim match {
        case x if x.length < 3 => S.error("Name too short")
        case x => {
          val pool = AccessPool.create.realm("Native").setName(name)
          pool match {
            case Failure(_,_,_) => S.error("Duplicate pool name!")
            case Full(p: AccessPool) => val privilegeSaved =
              Privilege.create.pool(p.saveMe).user(user).permission(Permission.Admin).save
              if(privilegeSaved) 
                S.notice("New pool added")
              else
                S.error("Could not add pool!")
            case _ => S.error("Could not add pool!")
          }
        }
      }

    }

    bind("add", in,
         "poolName" -> text("", addNewPool, "id" -> theInput)
    )
    
  }

  def editPool(in: NodeSeq): NodeSeq = {
    val redisplayPool = updatePool.is
    
    var pool = ""
    var username = ""
    val editPoolName = "edit_pool"
    val editUsername = "edit_username"
    val editPermission = "edit_permission"
    val adminUser = User.currentUser
    
    val adminPools = ("0", "--choose pool--") ::
    (adminUser match {
      case Full(u)=> Privilege.findAdminPools(u.id).map(
        p => (p.toString, AccessPool.find(p).get.getName)).toList
      case _ => Nil
    })
      
    val permissions = Permission.map(perm => (perm.id.toString, perm.toString)).collect
    
    def addPoolUser(permission: String): JsCmd = {
      val r: Box[Boolean] = 
      for (admin <- adminUser;
           p <- AccessPool.find(pool) ?~ "Pool not found";
           user <- User.findFromWeb(username) ?~ "User not found"
      ) yield if(Privilege.hasPermission(admin.id.is, p.id.is, Permission.Admin)) {
        val result = Privilege.create.user(user).pool(p).permission(Permission(permission.toInt)).save
        if (result) Distributor ! Distributor.AllowUserInPool(user.id.is, p.id.is)
        result
      } else false // "User has no permission to administer pool"
      r match {
        case Failure(m,_,_) => S.error(m)
        case Full(true) => S.notice("Successfully set user privileges in pool")
        case _ => S.error("Could not set user privileges in pool")
      }
      
      poolId.set(pool.toLong)
      redisplayPool() & SetValById(editUsername, "")
    }

    bind("edit", in,
         "pool" -> ajaxSelect(adminPools, Empty, p => {pool = p;
                                                       poolId.set(p.toLong);
                                                       redisplayPool()},
                                                 "id" -> editPoolName),
         "username" -> text(username, username = _, "id" -> editUsername),
         "permission" -> select(permissions, Empty, addPoolUser, "id" -> editPermission)
    )
    
  }
  
  def displayPoolUsers(in: NodeSeq): NodeSeq = {
    // get the span name to update
    val spanName = S.attr("the_id") openOr "PoolSpan"
    // get the current user
    val user = User.currentUser

    def doRender(): NodeSeq =
    Privilege.findAll(By(Privilege.pool, poolId.is)) match {
      case Nil => NodeSeq.Empty
      case xs => bind("pool", in,
                      "user" -> 
                      (lst => xs.flatMap(i => bind("user", lst,
                                                   "name" -> User.find(i.user).get.nickname.is,
                                                   "privilege" -> i.permission.is.toString
                      ))))
    }
    

    def updateSpan(): JsCmd = SetHtml(spanName, doRender())

    updatePool.set(updateSpan)
    doRender
 }
}
