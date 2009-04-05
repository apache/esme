package org.apache.esme.model

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

import net.liftweb._
import mapper._
import util._

object Privilege extends Privilege with LongKeyedMetaMapper[Privilege] {

}

class Privilege extends LongKeyedMapper[Privilege] {
  def getSingleton = Privilege // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object pool extends MappedLongForeignKey(this, AccessPool)
  object user extends MappedLongForeignKey(this, User)
  object permission extends MappedString(this, 256) with Permission
  
  sealed trait Permission
  case object Read extends Permission {
    def toStr = "Read"
  }
  case object Write extends Permission {
    def toStr = "Write"
  }
  case object Admin extends Permission {
    def toStr = "Admin"
  }
}
