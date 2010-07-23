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
//import http._         
import mapper._
//import util._
//import actor._
//import common._
//import Helpers._  

object UserTagFollow extends UserTagFollow with LongKeyedMetaMapper[UserTagFollow]

class UserTagFollow extends LongKeyedMapper[UserTagFollow] with IdPK {  
  def getSingleton = UserTagFollow

  object user extends LongMappedMapper(this, User)
  object tag extends LongMappedMapper(this, Tag)
}