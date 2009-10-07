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
import http._
import util._

import scala.xml.Text

import java.util.Date

object AccessPool extends AccessPool with LongKeyedMetaMapper[AccessPool] {
  val Native = "Native"

  def findPool(name: String, realm: String): Box[AccessPool] = 
    AccessPool.find(By(AccessPool.name,  name),
                    By(AccessPool.realm, realm))

  //set createdDate and creator when instance AccessPool
  override def create: AccessPool = {
    val ap = super.create
    ap.createdDate(new Date())
    ap.creator(User.currentUser)
    ap
  }
}

class AccessPool extends LongKeyedMapper[AccessPool] {
  def getSingleton = AccessPool
  def primaryKeyField = id

  object id extends MappedLongIndex(this)

  // is it worth having foreign key to another table?
  object realm extends MappedString(this, 256)

  private[model] object name extends MappedString(this, 256) {
    
    override def validations = checkDuplicate _ :: super.validations
    
    def checkDuplicate(in: String): List[FieldError] = 
      sameName(in).map(p =>
        FieldError(this, Text("Duplicate pool: " + in + " in realm " + p.realm.is ))
      )
    
  }
  
  // set modify information when setName
  def setName(in: String) = sameName(in) match {
    case Nil => {
      Full(this.name(in))
      Full(this.lastModifyDate(new Date()))
      Full(this.modifier(User.currentUser))
    }
    case List(_,_*) => Failure("Duplicate access pool name!")
  }
  
  def getName() = name.is

  private def sameName(name: String) = 
    AccessPool.findAll(By(AccessPool.name, name)).
      filter(_.realm.is.equalsIgnoreCase(this.realm.is))
  
  //define create and modify fields
  object createdDate extends MappedDateTime(this) 
  object creator extends MappedLongForeignKey(this,User)
  object lastModifyDate extends MappedDateTime(this) 
  object modifier extends MappedLongForeignKey(this,User)
  
}
