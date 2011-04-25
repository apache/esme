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
 * function to display new_pool modal dialog
 */
 
 

	$(function() {
		$("#dialog").dialog("destroy");
		
		var pool_input = $("#new_pool"),
         new_pool_description = $("#new_pool_description"),
		  edit_username = $("#edit_username"),
		   edit_permission = $("#edit_permission"),
			allFields = $([]).add(pool_input),
			allFieldsUser = $([]).add(edit_username).add(edit_permission),
			tips = $(".validateTips");

		function updateTips(t) {
			tips
				.text(t)
				.addClass('ui-state-highlight');
			setTimeout(function() {
				tips.removeClass('ui-state-highlight', 1500);
			}, 500);
		}
		


		function checkLength(o,n,min,max) {

			if ( o.val().length > max || o.val().length < min ) {
				o.addClass('ui-state-error');
				updateTips("Length of " + n + " must be between "+min+" and "+max+".");
				return false;
			} else {
				o.removeClass('ui-state-error');
                tips
                  .text("")
                  .removeClass('ui-state-highlight');
				return true;
			}

		}

		function checkRegexp(o,regexp,n) {

			if ( !( regexp.test( o.val() ) ) ) {
				o.addClass('ui-state-error');
				updateTips(n);
				return false;
			} else {
				o.removeClass('ui-state-error');
                tips
                  .text("")
                  .removeClass('ui-state-highlight');
				return true;
			}

		}
		
		$("#dialog-form").dialog({
			autoOpen: false,
			height: 300,
			width: 350,
			modal: true,
			buttons: {
				'Create': function() {
					var bValid = true;
					allFields.removeClass('ui-state-error');

					bValid = bValid && checkLength(pool_input,"Pool name",3,16);

					bValid = bValid && checkRegexp(pool_input,/^[a-z]([0-9a-z_])+$/i,"Name may consist of a-z, 0-9, underscores, begin with a letter.");

               npdValid = checkLength(new_pool_description, "Description", 0, 64);
					
					if (bValid && npdValid ) {
						 liftAjax.lift_ajaxHandler(jQuery("#pool_new_dialog").serialize(), null, null, "javascript");  

						$(this).dialog('close');
					}
				},
				Cancel: function() {
					$(this).dialog('close');
				}
			},
			close: function() {
				allFields.val('').removeClass('ui-state-error');
			}
		});
		
		$("#dialog-form-user").dialog({
			autoOpen: false,
			height: 300,
			width: 350,
			modal: true,
			buttons: {
				'Add a user to a pool': function() {
					var bValid = true;
					allFieldsUser.removeClass('ui-state-error');

                    var editUsername = $("input[name='edit_username']")
					bValid = bValid && checkLength(editUsername,"User name",2,16);
					bValid = bValid && checkRegexp(editUsername,/^[a-z]([0-9a-z_])+$/i,"Name may consist of a-z, 0-9, underscores, begin with a letter.");
					
					if (bValid) {
						 liftAjax.lift_ajaxHandler(jQuery("#pool_new_user_dialog").serialize(), null, null, "javascript");  

						$(this).dialog('close');
					}
				},
				Cancel: function() {
					$(this).dialog('close');
				}
			},
			close: function() {
				allFields.val('').removeClass('ui-state-error');
			}
		});

		
		
	});

// ]]>

