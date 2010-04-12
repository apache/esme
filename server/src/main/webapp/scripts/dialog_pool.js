// <![CDATA[
/*
 * function to display new_pool modal dialog
 */
 
 

	$(function() {
		$("#dialog").dialog("destroy");
		
		var pool_input = $("#new_pool"),
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
				return true;
			}

		}

		function checkRegexp(o,regexp,n) {

			if ( !( regexp.test( o.val() ) ) ) {
				o.addClass('ui-state-error');
				updateTips(n);
				return false;
			} else {
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
					
					if (bValid) {
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

					bValid = bValid && checkLength(edit_username,"User name",3,16);
					bValid = bValid && checkRegexp(edit_username,/^[a-z]([0-9a-z_])+$/i,"Name may consist of a-z, 0-9, underscores, begin with a letter.");
					
					if (bValid) {
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

		
		
	});

// ]]>

