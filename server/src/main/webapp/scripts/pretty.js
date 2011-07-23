/*
 * JavaScript Pretty Date
 * Copyright (c) 2008 John Resig (jquery.com)
 * Licensed under the MIT license.
 */

// Takes an ISO time and returns a string representing how
// long ago the date represents.
function prettyDate(myDate){
	var date = myDate,
		diff = (((new Date()).getTime() - date.getTime()) / 1000),
		day_diff = Math.floor(diff / 86400);
		
	
			
	if ( isNaN(day_diff) || day_diff < 0 )
		return;
		
	if ( day_diff >= 31 )
		return "more than 1 month ago";
			
	return day_diff == 0 && (
			diff < 60 && "just now" ||
			diff < 120 && "1 minute ago" ||
			diff < 3600 && Math.floor( diff / 60 ) + " minutes ago" ||
			diff < 7200 && "1 hour ago" ||
			diff < 86400 && Math.floor( diff / 3600 ) + " hours ago") ||
		day_diff == 1 && "Yesterday" ||
		day_diff < 7 && day_diff + " days ago" ||
		day_diff < 31 && Math.ceil( day_diff / 7 ) + " weeks ago";
}

// If jQuery is included in the page, adds a jQuery plugin to handle it as well
if ( typeof jQuery != "undefined" )
	jQuery.fn.prettyDate = function(){
		return this.each(function(){
			var date = prettyDate(this.title);
			if ( date )
				jQuery(this).text( date );
		});
	};
	
// Added by Ethan
// Find all divs with class "supp_data" and populate the child div "supp_date"
// based on the contents of the child div "supp_isodate".

function calculateDates() {
  var message_date = "";
  
  jQuery(".supp_data").each(function(){
    message_date = prettyDate(new Date($(this).children(".supp_millidate").html()));
    $(this).children(".supp_date").html(message_date);
  });
}    
              
jQuery(document).ready(function(){
  calculateDates();
});
window.setInterval(calculateDates, 30000);


