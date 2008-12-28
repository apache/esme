
/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

package us.esme.lib

import us.esme._
import model._

import scala.xml.{NodeSeq, Node, Text}

import net.liftweb._
import util._
import http._
import Helpers._

import java.text.SimpleDateFormat

object IsIE7 extends SessionVar[Boolean]({
    // "Firefox/3"
    // "MSIE 7"
    def findUserAgent(in: List[(String, String)]): Box[String] =
    in.filter(_._1.equalsIgnoreCase("User-Agent")).map(_._2).firstOption

  val r: Box[Boolean] =
  for (session <- S.session;
       agent <- findUserAgent(session.initialHeaders)) yield
  agent.indexOf("MSIE 7") >= 0

  r openOr false
})

trait MsgFormat {
  
  def formatMsg(in: Message, showReply: Boolean, showConv: Boolean): NodeSeq =
  formatMsg(in, showReply, showConv, Empty)

  def formatMsg(in: Message, showReply: Boolean, showConv: Boolean,
                inside: Box[() => NodeSeq]): NodeSeq =
  <div class="b-list">
    <table>
      <tr>
        <td class="image">{
            val r: Box[NodeSeq] =
            for (user <- in.author.obj;
                 image <- user.image if !IsIE7.is) yield <img src={image} alt={user.niceName}/>

            r openOr <xml:group>&nbsp;</xml:group>
          }</td>
        <td class="message">
          <div class="outer">
            <div class="inner clear">
              <p class="text">{in.digestedXHTML}</p>
            </div>
            <p class="date">{in.author.obj.map(u => <a href={"/user/"+urlEncode(u.nickname)}>{u.niceName}</a>).openOr("")}: 
              { dateFormat(in.when)} {
                if (showReply) <a href={"javascript:setReplyTo("+in.id+","+
                                        in.getText.encJs+
                                        ")"}>reply</a>
                else Text("")

              }
              {
                in.conversation.can.toList.map(cid => <a href={"/conversation/"+cid}>conversation</a>)
              }
            </p>
          </div>
        </td>
        <td class="tag">
          {
            in.tags.map(tag => <p><a href={"/tag/"+urlEncode(tag)}>{tag}</a></p>)
          }
        </td>
      </tr>
    </table>
    {
      inside.map(_()).openOr(Text(""))
    }
  </div>

  private def dateFormat(in: Long): String = {
    val delta = ((millis - in) / 1000L).toInt

    delta match {
      case 1 => "1 Second ago"
      case n if n < 60 => ""+n+" Seconds ago"
      case n if n >= 60 && n < 120 => "1 Minute ago"
      case n if n < 3600 => ""+(n / 60)+" Minutes ago"
      case n =>
        (n / 3600) match {
          case 1 => "1 Hour ago"
          case n if n < 24 => ""+n+" Hours ago"
          case _ => val df = new SimpleDateFormat("MM/dd HH:mm zzz")
            dateFormatter.format(new java.util.Date(in))
        }
    }
  }
}
