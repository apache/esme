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
import common._

import scala.xml._

object DidPerform extends DidPerform with LongKeyedMetaMapper[DidPerform] {

}

class DidPerform extends LongKeyedMapper[DidPerform] {
  def getSingleton = DidPerform // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  object what extends MappedLongForeignKey(this, Action)
  object message extends MappedLongForeignKey(this, Message)
  object when extends MappedDateTime(this) {
    override def defaultValue = Helpers.timeNow
  }
}
