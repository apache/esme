/*
 Licensed to the Apache Software Foundation (ASF) under one   *
 or more contributor license agreements.  See the NOTICE file *
 distributed with this work for additional information        *
 regarding copyright ownership.  The ASF licenses this file   *
 to you under the Apache License, Version 2.0 (the            *
 "License"); you may not use this file except in compliance   *
 with the License.  You may obtain a copy of the License at   *
                                                              *
   http://www.apache.org/licenses/LICENSE-2.0                 *
                                                              *
 Unless required by applicable law or agreed to in writing,   *
 software distributed under the License is distributed on an  *
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY       *
 KIND, either express or implied.  See the License for the    *
 specific language governing permissions and limitations      *
 under the License.                                           * 
*/

// <![CDATA[
/*
 * displayMessages called by lift:comet, type="Timeline" and type="PublicTimeline"
 */

function msgDateCompare(msg1, msg2)
{
  return parseInt(msg1.message.when) - parseInt(msg2.message.when);
}

// Replaces all instances of the given substring.
String.prototype.replaceAll = function( 
	strTarget, // The substring you want to replace
	strSubString // The string you want to replace in.
	){
	var strText = this;
	var intIndexOfMatch = strText.indexOf( strTarget );
	 
	// Keep looping while an instance of the target string
	// still exists in the string.
	while (intIndexOfMatch != -1){
		// Relace out the current instance.
		strText = strText.replace( strTarget, strSubString )
		 
		// Get the index of any next matching substring.
		intIndexOfMatch = strText.indexOf( strTarget );
	}
	 
	// Return the updated string with ALL the target strings
	// replaced out with the new substring.
	return( strText );
}


function displayMessages(msgArray, elementId)
{
	

	
 // Select the first element in table id="timeline_messages"
  //  with id="message" as the message template
  if (msgTemplate == null) {
    //                                    var msgTemplate = jQuery('span.'+spanId+' message:first');
    var msgTemplate = jQuery('#'+elementId+' #message:first');
    var tagTemplate = msgTemplate.find('#tag:first');
    var msgInsertPt = jQuery('#'+elementId);

    // Now we have the template, make the existing instances invisible
    jQuery('#'+elementId+' *[id=message]').hide();
  }

  // Sort the messages into date order
  msgArray.sort(msgDateCompare);

  for (var msgIndex in msgArray)
  {
    // Marshall the data from the Comet-supplied message
    var cometMsg = msgArray[msgIndex].message;
    var cometReason = msgArray[msgIndex].reason;
    var cometResent = msgArray[msgIndex].resent;
    var msgId = "message_"+cometMsg.id;

    // Only do this if the message is not already in the table
    if (jQuery('#'+elementId+' #'+msgId).size() == 0)
    {
      var msgAuthor = cometMsg.author;
      var msgBody = jQuery(cometMsg.text).find('body').html();
      var msgDateObj = new Date(parseInt(cometMsg.when));
      
      if (!msgBody)
      	msgBody = cometMsg.text;
      
      var msgDateStr = prettyDate(msgDateObj);
      

      
      var msgPool = '';
      if (cometMsg.pool) msgPool = 'in pool ' + cometMsg.pool.name; 
      var msgPoolId = 0;
      if (cometMsg.pool) msgPoolId = cometMsg.pool.id; 
      var msgSource = cometMsg.source;
      var msgConversation = cometMsg.conversation;
      var msgReason = ""
      for (r in cometReason) {
      	msgSource = ""
        if (r == "resent_from")
          msgReason = "resent by " + cometReason[r].nickname;
        else
          msgReason = "caused by " + r;
        break
      }
      var msgTags = jQuery(cometMsg.text).find('tags > tag').get();
      for (var tagIndex=0; tagIndex < msgTags.length; tagIndex++) {
        // Replace each tag element with the plain tag text
        msgTags[tagIndex] = jQuery(msgTags[tagIndex]).attr('name');
      }

      // Put the marshalled data into a copy of the template
      var newMsg = msgTemplate.clone(true).attr('id',msgId);

      newMsg.find('#author').text(msgAuthor.nickname);
      newMsg.find('#author').attr('href', "/user/" + msgAuthor.nickname );
     
     
      // Dealing with users with no avatars
      if (!msgAuthor.imageUrl) {
      	 msgAuthor.imageUrl= "images/avatar.jpg"
     }
     
     	
     if (!msgPool)
      	msgPool="public"
      	
      var avatar = newMsg.find('#avatar')
      .attr('src', msgAuthor.imageUrl)
      .attr('alt',msgAuthor.firstname + ' ' + msgAuthor.lastname);
      
      newMsg.find('#body').html(msgBody);
      newMsg.find('#supp_data').text(msgPool + " " + msgDateStr  + " " +  msgReason  + " " +   msgSource);
      newMsg.find('#msgPool').text(msgPool);
      //newMsg.find('#reason').text(msgReason);
      //newMsg.find('#when').text(msgDateStr);
      var id = cometMsg.id;

      var resendButton = newMsg.find('#resend');
      if (cometResent) {
        resendButton.css("display", "none");
      } else {
        resendButton.attr('id', 'resend_' + id).
          attr('onclick', 'javascript:resend_msg(' + id + ');' +
                                     'clearResend("resend_' + id + '")');
      }

      var tempStr = msgBody.replaceAll ("'", ".");
      
      var myReplyMsg = tempStr.replaceAll (".", "\'");
          
      newMsg.find('#reply').attr('href',
        "javascript:setReplyTo(" + id + ", '"+ myReplyMsg + "'," + msgPoolId + ", '" + msgAuthor.nickname + "')");
      var conversation = newMsg.find('#conversation');
      if (msgConversation != 0) {
        conversation.attr('href', 
          '/conversation/' + msgConversation);
      } else {
        conversation.css("display", "none");
      }
      for (var tagIndex=0; tagIndex < msgTags.length; tagIndex++) {
        var newTag = tagTemplate.clone(true).attr('id',msgTags[tagIndex]);
        newTag.find('a')
        .attr('href','tag/'+msgTags[tagIndex])
        .text(msgTags[tagIndex]);
        newTag.insertBefore(newMsg.find('#tag:first'));
      }

      // Remove any old tags from the template
      newMsg.find('*[id=tag]').remove();

      // Insert the updated copy of the message into the page
      newMsg.prependTo(msgInsertPt).show();
    }
  }
}
// ]]