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

package org.apache.esme.lib

import org.apache.esme._
import model._
import org.apache.esme.actor._

import net.liftweb._
import http._
import SHtml._
import js._
import JsCmds._
import JE._
import util._
import common._
import Helpers._

import mapper._

import scala.xml.{NodeSeq, Text}
import java.util.Date

object MessageUtils {
  def bindMessages(messageList: List[Message])(in: NodeSeq): NodeSeq =
    messageList.flatMap{m =>
      val nickname = m.author.obj.map(_.nickname.is) openOr ""
      bind("item", in,
           "author" -> <a href={"/user/"+urlEncode(nickname)}>{nickname}</a>,
           "body" -> m.digestedXHTML,
           "date" -> Text(new Date(m.when.is).toString))
    }          
}