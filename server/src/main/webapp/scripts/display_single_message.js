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
function login()
{
  var myAction;
  	
  if (document.forms[0].open_id.value != "")
     document.forms[0].action = 'open_id/login';
  else if(jQuery('input#ldapEnabled').val() == 'true')
     document.forms[0].action = 'ldap/login';
  else
     document.forms[0].action = 'authentication/login';


  document.forms[0].submit();	
}                      
// ]]>