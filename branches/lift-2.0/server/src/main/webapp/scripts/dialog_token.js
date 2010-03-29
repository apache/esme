// <![CDATA[
/*
 * function to display token modal dialog
 */
 
 

	$(function() {
		$("#dialog").dialog("destroy");
		
		var token_input = $("#token_input"),
			allFields = $([]).add(token_input),
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
				'Create a new token': function() {
					var bValid = true;
					allFields.removeClass('ui-state-error');

					bValid = bValid && checkLength(token_input,"Token",3,16);

					bValid = bValid && checkRegexp(token_input,/^[a-z]([0-9a-z_])+$/i,"Token may consist of a-z, 0-9, underscores, begin with a letter.");
					
					if (bValid) {
						 liftAjax.lift_ajaxHandler(jQuery("#token_new_dialog").serialize(), null, null, "javascript");  

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

