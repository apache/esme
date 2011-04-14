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

var currentConvNumber = 0;

function setReplyTo(id, text, msgPool, author){
    currentConvNumber = id;
     jQuery('#vMsg').focus();
    document.getElementById('reply-to-div').style.display = "block";
    if (author.length > 0) {
      jQuery('#message_request').html("Reply to: " + author);
    } else {
      jQuery('#message_request').html("Reply to conversation");
    }
    var rep_msg = text 
    if (text.length > 50)
     rep_msg = text.substr(0, 47) + "..."
    jQuery('#reply-to-span').html(rep_msg);
    if (author.length > 0) {
      jQuery('#vMsg').val("@" + author + " ")	
    }
    jQuery('#vMsg').focus();
    setCaretToPos(jQuery('#vMsg'), jQuery('#vMsg').val.length);
    jQuery('#vPool').val(msgPool);
   
}

function setSelectionRange(input, selectionStart, selectionEnd) {
  if (input.setSelectionRange) {
    input.focus();
    input.setSelectionRange(selectionStart, selectionEnd);
  }
  else if (input.createTextRange) {
    var range = input.createTextRange();
    range.collapse(true);
    range.moveEnd('character', selectionEnd);
    range.moveStart('character', selectionStart);
    range.select();
    
  }
}

function setCaretToPos (input, pos) {
  setSelectionRange(input, pos, pos);
}

function clearReplyTo(){
  currentConvNumber = 0;
  document.getElementById('reply-to-div').style.display = "none";
  jQuery('#vPool').val(0);
  jQuery('#message_request').html('What are you working on?');
}                            
// ]]