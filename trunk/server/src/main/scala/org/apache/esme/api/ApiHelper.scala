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
 * ApiHelper.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */                                    

/* 
 * This code stolen shamelessly and modified.  Based on the 
 * XMLApiHelper.scala in Liftweb 1.0
 */

package org.apache.esme.api          

import net.liftweb._ 
import http._
import util._
import common._
import Helpers._

import scala.xml.{NodeSeq, Text, Elem, UnprefixedAttribute, Null, Node}  
  
trait ApiHelper {  
  /**
   * This method converts from Rack/WSGI response format into the correct Lift response
   */

  implicit def rackResponse(in: Box[Tuple3[Int,Map[String,String],Box[Elem]]]): LiftResponse = in match {
    case Full((200,_,xml)) => buildResponse(true, Empty, xml openOr Text(""))
    case Full((403,_,_)) => ForbiddenResponse()
    case Full((404,_,_)) => NotFoundResponse()
    case _ => InternalServerErrorResponse()   
  }  
  
  implicit def putResponseInBox(in: LiftResponse): Box[LiftResponse] = Full(in)

  /** 
   * The method that wraps the outer-most tag around the body 
   */   

  def createTag(in: NodeSeq): Elem  
  
  /** 
   * Build the Response based on the body 
   */  

  protected def buildResponse(success: Boolean, msg: Box[NodeSeq],  
                            body: NodeSeq): LiftResponse = 
    XmlResponse(createTag(body))

}
