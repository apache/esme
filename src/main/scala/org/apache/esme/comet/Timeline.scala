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

package org.apache.esme.comet

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml._
import js._
import JsCmds._
import JE._      
import net.liftweb.http.js.jquery.JqJsCmds.{PrependHtml} 

import org.apache.esme._
import org.apache.esme.actor._
import model._
import lib._

import java.text._

trait Timeline extends CometActor {   
                                                                           
  protected var messages: List[(Long,MailboxReason,Boolean)] = Nil  
  protected var clearMessages: Boolean = false  
  protected val jsId: String  
  
  override def defaultPrefix = Full("timeline")
  
  override def localSetup() {  
    super.localSetup()
  }
  
  override def localShutdown() {
    super.localShutdown()
  }
  
  def render = { 
  
// TODO - handle clearMessages = true.   
// TODO - Get resend working    
// TODO - need to escape the replyHref string so that messages with ? in them don't bomb
  
    val msgMap = Message.findMessages(messages map {_._1})
    val toDisplay = for ((id, reason, resent) <- messages;
                         msg <- msgMap.get(id))
                    yield (msg, reason, resent)    
                    
    <div id={jsId}>{toDisplay.map(renderMessage(_))}</div>
  }  
  
  protected def renderMessage(m: (Message,MailboxReason,Boolean)) = {
    val imageUrl = m._1.author.obj.map(_.image_url).openOr("")       
    val authorNickname = m._1.author.obj.map(_.niceName).openOr("")
    val messageId = "message_" + m._1.id.is.toString
    val messageBody = m._1.digestedXHTML
    val messagePool:String = m._1.pool.obj.map("in pool \'" + _.getName + "\'").openOr("")  
    val replyHref = "javascript:setReplyTo(" + m._1.id.is.toString + ", '"+ messageBody + "', " + m._1.pool.obj.map(_.id.is).openOr(0) + ", '" + authorNickname + "')" 
                              
    val convId = m._1.conversation.is  
    val convHref = LiftRules.context.path + "/conversation/" + convId
    val convTransform:CssBindFunc = if(convId != 0) {
      ".conversation [href]" #> convHref
    } else {
      ".conversation" #> Text("")
    }   
                        
    val authorHref = LiftRules.context.path + "/user/" + authorNickname
    
// TODO: Put date in the "ago" format
    val messageDateStr = toInternetDate(m._1.when)
    val messageReason = if(m._2.attr.length > 0){
      if(m._2.attr.key == "resent_from") {
        "resent by " + User.find(m._2.attr.value).map(_.nickname).openOr("")
      } else {
        "caused by " + m._2.attr.key
      }
    } else {
      "via " + m._1.source
    }
      
    val suppString = messagePool + " " + messageDateStr + " " + messageReason
  
    ("#avatar [src]" #> imageUrl &
     ".updates-box [id]" #> messageId &
     ".msgbody *" #> messageBody &
     ".supp_data *" #> suppString &
     ".reply [href]" #> replyHref &
     convTransform &       
     ".author [href]" #> authorHref &
     ".author *" #> authorNickname )(messageTemplate)
  }                                                 
  
  // If we need to filter out some messages on display, override this method.
  def filter(msgs:List[(Message,MailboxReason,Boolean)]):List[(Message,MailboxReason,Boolean)] = {
    msgs 
  }     
  
  protected def prependMessage(m:Message, r:MailboxReason, rs:Boolean) {     
    val newMessage = renderMessage((m,r,rs))    
    val update = PrependHtml(jsId, newMessage)
    partialUpdate(update)
  }

// TODO Should be factored out into a template  
  val messageTemplate = 
    <div class="updates-box" id="message"> 
    	<div class="avatar">
    		<img id="avatar" width="40px" src=""/>
    	</div>
    	<div class="update2">
    		<a href="" class="author"/>
    		<div class="msgbody"/>
      	<div class="supp_data"/>				
    		<div class="actions">
    			<a href="#"  class="resend">
    				<lift:loc>ui_messages_message_label_resend</lift:loc>
    			</a><span class="resend">| </span>
    			<a href="#" class="reply">
    				<lift:loc>ui_messages_message_label_reply</lift:loc>
    			</a>
    			<span class="conversation">| </span>
    			<a class="conversation">
    			<lift:loc>ui_messages_message_label_conversation</lift:loc>
    			</a>
    		</div>
    	</div>
    </div>   
}
