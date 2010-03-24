// <![CDATA[
/*
 * displayMessages called by lift:comet, type="Timeline" and type="PublicTimeline"
 */
 
 

	$(function() {
		// a workaround for a flaw in the demo system (http://dev.jqueryui.com/ticket/4375), ignore!
		$("#dialog").dialog("destroy");
		
		var tracking_input = $("#tracking_input"),
			allFields = $([]).add(tracking_input),
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
			height: 200,
			width: 350,
			modal: true,
			buttons: {
				'Create a new track': function() {
					var bValid = true;
					allFields.removeClass('ui-state-error');

					bValid = bValid && checkLength(tracking_input,"tracking_input",3,16);

					bValid = bValid && checkRegexp(tracking_input,/^[a-z]([0-9a-z_])+$/i,"Track may consist of a-z, 0-9, underscores, begin with a letter.");
					
					if (bValid) {
						 //$('button[type=submit] .default').click(); 
						 this.ownerDocument.forms[1].submit();   

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

