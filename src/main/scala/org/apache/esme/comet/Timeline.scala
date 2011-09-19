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
import net.liftweb.http.js.jquery.JqJsCmds.{PrependHtml,FadeOut}  
import net.liftweb.http.SHtml.{BasicElemAttr}

import org.apache.esme._
import org.apache.esme.actor._
import model._
import lib._

import java.text._

trait Timeline extends CometActor {   
                                                                           
  protected var messages: List[(Long,MailboxReason,Boolean)] = Nil  
  protected val jsId: String  
  
  override def defaultPrefix = Full("timeline")
  
  override def localSetup() {  
    super.localSetup()
  }
  
  override def localShutdown() {
    super.localShutdown()
  }
  
  def render = { 
                                        
    val msgMap = Message.findMessages(messages map {_._1})
    val toDisplay = for ((id, reason, resent) <- messages;
                         msg <- msgMap.get(id))
                    yield (msg, reason, resent)    
                    
    <div id={jsId}>{toDisplay.map(renderMessage(_))}</div>
  }                                                 
  
  // If we need to filter out some messages on display, override this method.
  def filter(msgs:List[(Message,MailboxReason,Boolean)]):List[(Message,MailboxReason,Boolean)] = {
    msgs 
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
      	<div class="supp_data">
      	  <span class="supp_pool"/>
      	  <span class="supp_date"/>
      	  <span class="supp_millidate" style="display:none"/>
      	  <span class="supp_reason"/>
      	</div>				
    		<div class="actions">
    			<a href="#"  class="resend resend_link">
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
    
  protected def prependMessage(m:Message, r:MailboxReason, rs:Boolean) {     
    val newMessage = renderMessage((m,r,rs))    
    val update = PrependHtml(jsId, newMessage) & Run("calculateDates();")
    partialUpdate(update)
  }       

  private def resendMessage(m:Message):JsCmd = {
    for (user <- User.currentUser;
         msgId = m.id.is) {
           Distributor ! Distributor.ResendMessage(user.id, msgId)       
    }        

    val resendId = "resend_" + m.id.toString

    FadeOut(resendId,0,200)
  }   
    
  protected def renderMessage(m: (Message,MailboxReason,Boolean)) = {
    val imageUrl = m._1.author.obj.map(_.image_url).openOr("")       
    val authorNickname = m._1.author.obj.map(_.niceName).openOr("")
    val messageId = "message_" + m._1.id.is.toString
    val messageBody = m._1.digestedXHTML  
    val replyBody = m._1.body.replaceAll("\\'","\\\\\\'")
    val messagePool:String = m._1.pool.obj.map("in pool \'" + _.getName + "\'").openOr("")  
    val replyHref = "javascript:setReplyTo(" + m._1.id.is.toString + ", '"+ replyBody + "', " + m._1.pool.obj.map(_.id.is).openOr(0) + ", '" + authorNickname + "')"             

    val convId = m._1.conversation.is  
    val convHref = LiftRules.context.path + "/conversation/" + convId
    val convTransform:CssBindFunc = if(convId != 0) {
      ".conversation [href]" #> convHref
    } else {
      ".conversation" #> Text("")
    }       

    val resendId = "resend_" + m._1.id.toString 
    val resendAttrs = BasicElemAttr("id",resendId).compose(BasicElemAttr("class","resend"))

    val resendTransform:CssBindFunc = 
      if(m._3 || m._1.author.is == User.currentUser.map(_.id.is).openOr(0)) {
        ".resend" #> Text("")
      } else {  
        ".resend_link" #> SHtml.a(
          () => resendMessage(m._1), 
          S.loc("ui_messages_message_label_resend").openOr(Text("")),
          resendAttrs)
      } 

      val authorHref = "/user/" + authorNickname
                                       
    val messageDate = toInternetDate(m._1.when)
    
    val messageReason = if(m._2.attr.length > 0){
      if(m._2.attr.key == "resent_from") {
        "resent by " + User.find(m._2.attr.value).map(_.nickname).openOr("")
      } else {
        "caused by " + m._2.attr.key
      }
    } else {
      "via " + m._1.source
    }                                                                          

    ("#avatar [src]" #> imageUrl &
     ".updates-box [id]" #> messageId &
     ".msgbody *" #> messageBody &
     ".supp_pool *" #> messagePool &
     ".supp_millidate *" #> messageDate &
     ".supp_reason *" #> messageReason &
     ".reply [href]" #> replyHref &
     convTransform &       
     ".author [href]" #> authorHref &
     ".author *" #> authorNickname &
     resendTransform )(messageTemplate)
  }   
}
