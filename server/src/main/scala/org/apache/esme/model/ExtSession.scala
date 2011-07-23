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
import mapper._
import util._
import common._

object ExtSession extends ExtSession with LongKeyedMetaMapper[ExtSession] with MetaProtoExtendedSession[ExtSession] {
  override def dbTableName = "ext_session" // define the DB table name

  def logUserIdIn(uid: String): Unit = User.logUserIdIn(uid)

  def recoverUserId: Box[String] = User.currentUserId

  type UserType = User
}

class ExtSession extends LongKeyedMapper[ExtSession] with ProtoExtendedSession[ExtSession] {
  def getSingleton = ExtSession // what's the "meta" server

}
