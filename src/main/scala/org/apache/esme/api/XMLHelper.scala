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

/*
 * XmlHelper.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */                                    

package org.apache.esme.api          

import net.liftweb._ 
import http._
import util._
import common._
import Helpers._ 

import org.apache.esme._
import model._

import scala.xml.{NodeSeq, Text, Elem, UnprefixedAttribute, Null, Node}  
  
trait XmlHelper { 
  protected def userToXml(user: User): Elem =
<user><id>{user.id.toString}</id><nickname>{user.niceName}</nickname><image>{user.image}</image><whole_name>{user.wholeName}</whole_name></user> 
	
  protected def msgToXml(msg: Message): Elem = 
<message>
  <id>{msg.id.toString}</id>
  <date>{toInternetDate(msg.when.is)}</date>
  <source>{msg.source.sourceAttr.getOrElse(Text(""))}</source>
  <body>{msg.body}</body>
  <tags>{msg.tags}</tags>
</message>        
}
