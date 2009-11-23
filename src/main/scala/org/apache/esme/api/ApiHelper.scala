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
  implicit def boolToResponse(in: Boolean): LiftResponse =  
  buildResponse(in, Empty, Text(""))  
  
  implicit def canBoolToResponse(in: Box[Boolean]): LiftResponse =  
  buildResponse(in openOr false, in match {  
      case Failure(msg, _, _) => Full(Text(msg))  
      case _ => Empty  
    }, Text(""))  
  
  implicit def pairToResponse(in: (Boolean, String)): LiftResponse =  
  buildResponse(in._1, Full(Text(in._2)), Text(""))  
  
  implicit def nodeSeqToResponse(in: NodeSeq): LiftResponse =  
  buildResponse(true, Empty, in)  
  
  implicit def listElemToResponse(in: Seq[Node]): LiftResponse =  
  buildResponse(true, Empty, in)

  implicit def elemToResponse(in: Elem): LiftResponse =
  buildResponseFromElem(true, Empty, in)
  
  implicit def canNodeToResponse(in: Box[NodeSeq]): LiftResponse = in match {  
    case Full(n) => buildResponse(true, Empty, n)  
    case Failure(msg, _, _) => buildResponse(false, Full(Text(msg)), Text(""))  
    case _ => buildResponse(false, Empty, Text(""))  
  }  
  
  implicit def putResponseInBox(in: LiftResponse): Box[LiftResponse] = Full(in)

  implicit def takeResponseOutOfBox(in: Box[LiftResponse]): LiftResponse =
	in openOr false                                                   
  
  /** 
   * The method that wraps the outer-most tag around the body 
   */  
  def createTag(in: NodeSeq): Elem  
  
  /** 
   * Build the Response based on the body 
   */  
  protected def buildResponse(success: Boolean, msg: Box[NodeSeq],  
                            body: NodeSeq): LiftResponse = {
    if(success) {
      XmlResponse(createTag(body))
    } else {
      XmlResponse(createTag(body))  // Need to return a 401 response here
    }
  }  
  
  protected def buildResponseFromElem(success: Boolean, msg: Box[NodeSeq], body: Elem): LiftResponse = {
  	XmlResponse(createTag(body))                                                                                   
  }
}
