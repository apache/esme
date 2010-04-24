// <![CDATA[
/*
 * function to display action modal dialog
 */
 
	$(function() {
		$("#dialog").dialog("destroy");
		
		var name_input = $("#name_input"),
		    test_input = $("#test_input"),
		    action_input = $("#action_input"), 
		    allFields = $([]).add(name_input).add(test_input).add(action_input),
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
			height: 450,
			width: 350,
			modal: true,
			buttons: {
				'Create': function() {
					var bValid = true;
					allFields.removeClass('ui-state-error');

					bValid = bValid && checkLength(name_input,"Name",3,64);
					bValid = bValid && checkRegexp(name_input,/^[a-z]([\w\s_])+$/i,"Name may consist of letters, digits, underscores, and spaces, begin with a letter.");
					
					if (bValid) {
						 liftAjax.lift_ajaxHandler(jQuery("#action_new_dialog").serialize(), null, null, "javascript");  

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

