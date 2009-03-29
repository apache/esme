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

package org.apache.esme.snippet

import net.liftweb._
import http._
import util._
import js._
import JE._
import JsCmds._
import Helpers._

import org.apache.esme._
import model._
import actor._

import scala.xml.{NodeSeq, Unparsed}


class Style {
  def header: NodeSeq = {
    Unparsed(
"""
    <!--[if gt IE 7]><!--><link rel="stylesheet" href='"""+
    S.contextPath+
    """/style/esme.css'/><!--<![endif]-->
    <!--[if lt IE 8]><link rel=stylesheet href='"""+
    S.contextPath+"""/style/esme-ie.css'><![endif]-->

"""
    )
  }
}
