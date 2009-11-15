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

package org.apache.esme.api

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import net.liftweb.common._
import org.specs.matcher._
import Helpers._
import net.sourceforge.jwebunit.junit.WebTester
import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.{Context, FilterHolder}
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.webapp.WebAppContext
import org.apache.esme._
import model._
import net.liftweb.http._
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework}

import net.sourceforge.jwebunit.junit.WebTester
import _root_.junit.framework.AssertionFailedError                 

class Api2SpecsAsTest extends JUnit3(Api2Specs)
object Api2SpecsRunner extends ConsoleRunner(Api2Specs)

object Api2Specs extends Specification with TestKit {
  JettyTestServer.start

  val baseUrl = JettyTestServer.urlFor("")

  implicit val reportError = new ReportFailure {
    def fail(msg: String): Nothing = Api2Specs.this.fail(msg)
  }
  
    def shouldnt(f: => Unit): Unit =
    try {
      val x = f
      fail("Shouldn't succeed")
    } catch {
      case _ => ()
    }
                         
  "allUsers" should {
    "have a response code of 200" in {
      API2.allUsers().toResponse.code must be equalTo(200)
    }

	"return a node listing all users" in {                    
	  API2.allUsers().toResponse.toString must include(
"""<api_response><users><user><id>1</id><nickname>hash</nickname><image>None</image><whole_name>hash</whole_name></user></users></api_response>"""
	  )                                         
	}
  }

                                    
}
 