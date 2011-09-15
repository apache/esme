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

import widgets.autocomplete.AutoComplete

import org.apache.esme._
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
  Menu(Loc("accessPools", new Link("pools_view" :: Nil, true)/*List("pools_view", "index")*/, S.?("base_pool_menu"), ifIsLoggedIn,
           Loc.Snippet("addPool", addPool _),
           Loc.Snippet("editPool", editPool _),
           Loc.Snippet("poolUsers", displayPoolUsers _),
  		   Loc.Snippet("poolDetail", displayPoolDetail _)//regist snippet for pool detail display
          ,Loc.Snippet("myPools", myPoolsWithRoles _)
  		)) ::
  Nil

  object updatePool extends RequestVar[() => JsCmd](() => Noop)
  
  //update pool detail response 
  object updatePoolDetail extends RequestVar[() => JsCmd](() => Noop)

  object poolsWithRoles extends RequestVar[() => JsCmd](() => Noop)

  object poolId extends RequestVar[Long](0)


  /*
  * Function for adding pools
  *
  */
  def addPool(in: NodeSeq): NodeSeq = {

    // redisplay my pools and roles
    val redisplayPoolsAndRoles = poolsWithRoles.is
  
    val theInput = "new_pool"
    val newPoolDescription = "new_pool_description";
    val user = User.currentUser
    var newPoolName = "";

      
    //def addNewPool(name: String) = {
    def addNewPool(poolDescription: String) = {

      /*
      name.trim match {
        case x if x.length < 3 => DisplayMessage("messages", <b>{S.?("base_pool_error_name_short")}</b>,  3 seconds, 3 seconds)
        case x => {
      */
      (newPoolName.trim, poolDescription.trim) match {
        case (x, y) if x.length < 3 => DisplayMessage("messages", <b>{S.?("base_pool_error_name_short")}</b>,  3 seconds, 3 seconds)
        case (x, y) => {
          import org.apache.esme.model.AccessPool
          //val pool = AccessPool.create.realm(AccessPool.Native).setName(name)
          val pool = AccessPool.create.realm(AccessPool.Native).setUpAccessPool(x, y)
          pool match {
            //case Failure(_,_,_) => DisplayMessage("messages", <b>{S.?("base_pool_msg_duplicate_name_pool",name)}</b>,  3 seconds, 2 seconds)
            case Failure(_,_,_) => DisplayMessage("messages", <b>{S.?("base_pool_msg_duplicate_name_pool",x)}</b>,  3 seconds, 2 seconds)
            case Full(p: AccessPool) => val privilegeSaved =
              Privilege.create.pool(p.saveMe).user(user).permission(Permission.Admin).save
              if(privilegeSaved && user.isDefined) {
                import org.apache.esme.model.{Privilege, User}
                Distributor ! Distributor.AllowUserInPool(user.get.id.is, p.id.is)
                //logger.info("ACCESS: " + S.?("base_pool_msg_new_pool",name))
                logger.info("ACCESS: " + S.?("base_pool_msg_new_pool",x))
                val selectPools = ("0", S.?("base_pool_msg_choose_pool")) ::
                (User.currentUser match {
                  case Full(u)=> Privilege.findViewablePools(u.id).map(
                    p => (p.toString, AccessPool.find(p).get.getName)).toList
                  case _ => Nil
                }).sortWith(_._2 < _._2)
                SetValById(theInput, "")  &
                ReplaceOptions("edit_pool", selectPools, Full(p.id.is.toString))  &
                FireOnchangeById("edit_pool") &
                redisplayPoolsAndRoles() &
                //DisplayMessage("messages", <b>{S.?("base_pool_msg_new_pool",name)}</b>,  3 seconds, 2 seconds)
                DisplayMessage("messages", <b>{S.?("base_pool_msg_new_pool",x)}</b>,  3 seconds, 2 seconds)
              } else
                DisplayMessage("messages", <b>{S.?("base_pool_msg_no_permission")}</b>,  3 seconds, 2 seconds)
            case _ => S.error(S.?("base_error_general"))
          }
        }
      }

    }

    case class FireOnchangeById(domElemId: String) extends JsCmd {
      def toJsCmd =
        """document.getElementById(""" + domElemId.encJs + """).onchange();"""
    }

    bind("add", in,
         //"poolName" -%> text("", addNewPool, "id" -> theInput),
         "poolName" -%> text("", newPoolName = _ , "id" -> theInput),
         "poolDescription" -%> textarea("", addNewPool, "id" -> newPoolDescription, "cols" -> "33", "rows" -> "2"))

  }

  var lastSelPool = "";

  /*
  * Function for editing pools
  *
  */
  def editPool(in: NodeSeq): NodeSeq = {
    import org.apache.esme.model.{AccessPool, User}
    val redisplayPool = updatePool.is

    // redisplay pool detail
    val redisplayPoolDetail = updatePoolDetail.is

    val redisplayPoolsAndRoles = poolsWithRoles.is

    // redisplay pool users and pool detail
    def redisplay(): JsCmd = {
      redisplayPoolDetail() & redisplayPool() & redisplayPoolsAndRoles()
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

    val permissions = Permission.values.map(perm => (perm.id.toString, perm.toString)).toSeq


      /*
       * Function for adding a user to a pool
       *
       */
    def addPoolUser(permission: String): JsCmd = {
      pool = lastSelPool;
      val r: Box[Boolean] =
      for (admin <- adminUser;
           p <- AccessPool.find(pool) ?~ S.?("base_pool_err_pool_not_found"); //DisplayMessage("messages", <b>{S.?("base_pool_err_pool_not_found")}</b>,  3 seconds, 2 seconds);
           user <- User.findFromWeb(username) ?~ S.?("base_pool_err_user_not_found") //DisplayMessage("messages", <b>{S.?("base_pool_err_user_not_found")}</b>,  3 seconds, 2 seconds)
      ) yield if(Privilege.hasPermission(admin.id.is, p.id.is, Permission.Admin)) {
        val result = try {
          import org.apache.esme.model.Permission
          logger.info("ACCESS: " + S.?("base_pool_msg_new_user",user.id.is, p.id.is, permission))
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
       /*
      r match {
        case Failure(m,_,_) => S.error(m)
        case Full(true) => S.notice(S.?("base_pool_msg_permission_set"))
        case _ => S.error(S.?("base_error_general"))
      }
      */

      poolId.set(pool.toLong)

      ( r match {
        case Failure(m,_,_) => DisplayMessage("messages", <b>{m}</b>,  3 seconds, 2 seconds)//S.error(m)
        case Full(true) => DisplayMessage("messages", <b>{S.?("base_pool_msg_permission_set")}</b>,  3 seconds, 2 seconds)//S.notice(S.?("base_pool_msg_permission_set"))
        case _ => DisplayMessage("messages", <b>{S.?("base_error_general")}</b>,  3 seconds, 2 seconds)//S.error(S.?("base_error_general"))
      } ) & 
      //we needn't redisplay pool detail when add a new user
      redisplayPool() & SetValById(editUsername, "") &
      redisplayPoolsAndRoles()
    }

    /*
    bind("edit", in,
         "pool" -%> ajaxSelect(adminPools, Empty, p => {pool = p;
                                                       poolId.set(p.toLong);
                                                       redisplay() //redisplay pooluser and pool detail
                                                       },
                                                 "id" -> editPoolName),
         "username" -%> text(username, username = _, "id" -> editUsername),
         "permission" -%> select(permissions, Empty, addPoolUser, "id" -> editPermission)
    )
    */

    /*
    bind("edit", in,
         "pool" -%> org.apache.esme.liftwebext.SHtml.ajaxUntrustedSelect(adminPools, Empty, (p:String) => {lastSelPool = p;  pool = p;
                                                       poolId.set(p.toLong);
                                                       redisplay() //redisplay pooluser and pool detail
                                                       },
                                                 "id" -> editPoolName),
         "username" -%> text(username, username = _, "id" -> editUsername),
         "permission" -%> select(permissions, Empty, addPoolUser, "id" -> editPermission)
    )
    */

    bind("edit", in,
         "pool" -%> org.apache.esme.liftwebext.SHtml.ajaxUntrustedSortedSelect(adminPools,
                                                       true,
                                                       Empty, (p:String) => {lastSelPool = p;  pool = p;
                                                       poolId.set(p.toLong);
                                                       redisplay() //redisplay pooluser and pool detail
                                                       },
                                                 "id" -> editPoolName),
         "username" -%> AutoComplete("", (current : String,limit : Int) =>
          User.findAll(Like(User.nickname, (current + "%"))).map(_.nickname.is),
          username = _ : String,
          List(("selectFirst", "false"), ("minChars", "1")),
          "name" -> editUsername),
         "permission" -%> select(permissions, Empty, addPoolUser, "id" -> editPermission)
    )
  }

  def myPoolsWithRoles(in: NodeSeq): NodeSeq = {

    // get the span name to update
    val spanName = S.attr("the_id") openOr "pool_membership_and_roles"

    def doRender(): NodeSeq = {

      val user = User.currentUser

      val userPools =
        (user match {
          case Full(u)=> Privilege.findViewablePools(u.id).map(
            p => (p, AccessPool.find(p).get.getName))
          case _ => Nil
        })

      val userPerms = userPools.map((x: (Long, String)) => Privilege.getPermissionString(user.open_!.id, x._1));
      val poolsAndRoles = userPools.map(_._2).zip(userPerms)

      bind("myPool", in,
          "pool" ->
             (in1 =>
                poolsAndRoles.flatMap((x: (String, String)) =>
                                            bind("pool", in1,
                                              "poolName" -> x._1,
                                              "role" -> x._2
               ))))
    }

    def updateSpan(): JsCmd = SetHtml(spanName, doRender())
    poolsWithRoles.set(updateSpan)
    doRender
  }

  def displayPoolDetail(in: NodeSeq): NodeSeq = {
    import org.apache.esme.model.AccessPool
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
        case Full(ap) => {         
          bind(
            "pool", in,
            "name" -> (if ( ap.description == "" ) ap.getName else ap.getName + " - " + ap.description),
            "creator" -> displayUserName( ap.creator ),
            "createdDate" -> getDateHtml(ap.createdDate),
            "modifier" -> displayUserName(ap.modifier),
            "lastModifyDate" -> getDateHtml(ap.lastModifyDate)) }
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
      case xs => {
        def userNamePrivilege(lst : NodeSeq) : NodeSeq = {
          xs.flatMap(i => bind("user", lst,
                               "name" -> User.find(i.user).map(
                                         _.nickname.is).getOrElse(""),
                               "privilege" -> i.permission.is.toString
                          ))
        }

        def renderEditButton(in : NodeSeq) : NodeSeq = if(Privilege.findAdminPools(user.open_!.id).contains(poolId.is)) in else NodeSeq.Empty

        bind("pool", in,
          "user" -> userNamePrivilege _,
          "action" -> renderEditButton _)
      }
    }
    }

    def updateSpan(): JsCmd = SetHtml(spanName, doRender())

    updatePool.set(updateSpan)
    doRender
 }
}
