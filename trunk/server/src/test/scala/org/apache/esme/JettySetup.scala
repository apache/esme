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

package org.apache.esme

import _root_.junit.framework.AssertionFailedError
import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import net.liftweb.common._
import org.specs.matcher._
import Helpers._                                    
import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.{Context, FilterHolder}
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.webapp.WebAppContext
import org.apache.esme._
import model._
import net.liftweb.http._


object JettyTestServer {
  private val serverPort_ = System.getProperty("SERVLET_PORT", "8989").toInt
  private var baseUrl_ = "http://127.0.0.1:" + serverPort_

  System.setProperty("run.mode", "test")

  private val server_ : Server = {
    val server = new Server(serverPort_)

    val context = new WebAppContext()
    context.setServer(server)
    context.setContextPath("/")
    context.setWar("src/main/webapp")

    server.addHandler(context)

    server
  }

  def urlFor(path: String) = baseUrl_ + path

  lazy val start = {
    server_.start()
    val session = new LiftSession(Helpers.randomString(20), "", Empty)

    S.initIfUninitted(session) {
      User.create.nickname("hash").save
    }
  }

  def stop() = {
    server_.stop()
    server_.join()
  }               
}


