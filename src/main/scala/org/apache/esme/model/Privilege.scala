package org.apache.esme.model

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
import common._
import Helpers._

import org.apache.esme._
import model._

object Privilege extends Privilege with LongKeyedMetaMapper[Privilege] {
  
  override def beforeSave = deleteExisting _ :: super.beforeSave

  private def deleteExisting(in: Privilege) {
    // Delete current privileges of user in pool only
    // if admin permissions by other users exist
    if (in.permission.is == Permission.Admin ||
        find(By(pool, in.pool),
             By(permission, Permission.Admin),
             NotBy(user, in.user)).
        isDefined
    ) bulkDelete_!!(By(pool, in.pool),
                    By(user, in.user))
    else throw new Exception(S.?("base_pool_err_no_admin_users"))
                    
  }
  
  def findViewablePools(userId: Long): List[Long] =
  List(Privilege.findMap(
    By(Privilege.user, userId),
    NotBy(Privilege.permission, Permission.Denied)
  )(p => Full(p.pool.is)) :_*)

  def findWritablePools(userId: Long): List[Long] = List(Privilege.findMap(
    By(Privilege.user, userId),
    NotBy(Privilege.permission, Permission.Read),
    NotBy(Privilege.permission, Permission.Denied)
  )(p => Full(p.pool.is)) :_*)

  def findAdminPools(userId: Long): List[Long] = List(Privilege.findMap(
    By(Privilege.user, userId),
    By(Privilege.permission, Permission.Admin)
  )(p => Full(p.pool.is)) :_*)

  def getPermissionString(userId: Long, poolId: Long) : String = Privilege.find(
    By(user, userId),
    By(pool, poolId)
  ).map(_.permission.is).open_!.toString 
}

/**
 * A Privilege defines a permission for a certain user in an access pool.
 * This acts as a join table for the user:pool many-to-many relationship.
 *
 * Currently a user may only have one privilege type for a pool, where one
 * permission type will also include all rights for more restricted ones,
 * e.g. Write permission also includes Read permissions.
 *
 * In order to change to multiple Privileges per user or add another type,
 * redefining the find* methods from the companion object should be all
 * that is necessary.
 */
class Privilege extends LongKeyedMapper[Privilege] {
  def getSingleton = Privilege // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object pool extends MappedLongForeignKey(this, AccessPool)
  object user extends MappedLongForeignKey(this, User)
  object permission extends MappedEnum(this, Permission)
  
  def hasPermission(userId: Long, poolId: Long, permission: Permission.Value) = Privilege.find(
    By(user, userId),
    By(pool, poolId)
  ).map(_.permission.is >= permission).getOrElse(false)

}

/**
 * Types of permissions a user can have a Privilege to have
 */
object Permission extends Enumeration {
  val Read = Value(0, "Read")
  val Write = Value(1, "Read/Write")
  val Admin = Value(2, "Admin")
  val Denied = Value(3, "Denied")
}

