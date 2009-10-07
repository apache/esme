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

package org.apache.esme.actor

import java.io.PrintWriter
import scala.tools.nsc.Interpreter
import scala.tools.nsc.Settings

import scala.actors.Actor
import scala.actors.Actor._

import org.apache.esme.model._
import net.liftweb.util.{Empty,Props}

object ScalaInterpreter extends Actor{

  val settings = new Settings()
  val origBootclasspath = settings.bootclasspath.value
  lazy val pathList = List(jarPathOfClass("scala.tools.nsc.Interpreter"),
                           jarPathOfClass("scala.ScalaObject"))

  def act = loop {
    react {
      case ScalaExcerpt(from, replyTo, pool, code) =>
      settings.bootclasspath.value = (origBootclasspath :: pathList).mkString(java.io.File.separator)
      actor {
        val out = new java.io.StringWriter()
        val interpreter = new Interpreter(settings, new PrintWriter(out))
        interpreter.interpret(code)
        Message.create.author(from).
                       when(System.currentTimeMillis).
                       source("scala").
                       replyTo(replyTo).
                       pool(pool).
                       setTextAndTags(out.toString, Nil, Empty).
                       foreach{ msg =>
                         if (msg.save) {
                           Distributor ! Distributor.AddMessageToMailbox(from, msg, InterpreterReason(from))
                         }
                       }
      }
        
    }
  }
  
  if (Props.getBool("actions.scala_interpreter.enable", false))
    start
  
  // do nothing
  def touch {
  }
  
  def jarPathOfClass(className: String) = {
    val resource = className.split('.').mkString("/", "/", ".class")
    val path = getClass.getResource(resource).getPath
    val indexOfFile = path.indexOf("file:")
    val indexOfSeparator = path.lastIndexOf('!')
    path.substring(indexOfFile, indexOfSeparator)

    // potentially problematic with e.g. OSGi:
    // Class.forName(className).getProtectionDomain.getCodeSource.getLocation
  }
 
  case class ScalaExcerpt(from: Long, replyTo: Long, pool: Long, code: String)
}
