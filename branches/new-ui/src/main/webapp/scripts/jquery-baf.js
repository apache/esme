$(document).ready(function(){ 
		
		$("table:not([@color_odd_rows=false]) > tr:nth-child(odd)").addClass("odd");
        
        if($("#gametable_lhs").attr("id")=="gametable_lhs")
        {
            // adjust the top row height to be the same between the LHS and RHS tables
            var lhsHeight = ($("#gametable_lhs tr.gametable_header").attr("clientHeight"))?($("#gametable_lhs tr.gametable_header").attr("clientHeight")):(parseInt($("#gametable_lhs tr.gametable_header").css("height")));
            var rhsHeight = ($("#gametable_rhs tr.gametable_header").attr("clientHeight"))?($("#gametable_rhs tr.gametable_header").attr("clientHeight")):(parseInt($("#gametable_rhs tr.gametable_header").css("height")));
            if(lhsHeight > rhsHeight)
            {
                ($("#gametable_lhs tr.gametable_header th").css( "height", lhsHeight + "px" ));
                ($("#gametable_rhs tr.gametable_header th").css( "height", lhsHeight + "px" ));
            }
            else
            {
                ($("#gametable_lhs tr.gametable_header th").css( "height", rhsHeight + "px" ));
                ($("#gametable_rhs tr.gametable_header th").css( "height", rhsHeight + "px" ));
            }
            
            //adjust the "iconlist" row to be the same between the LHS and RHS tables
            var lhsIconHeight = ($("#gametable_lhs tr.iconlist").attr("clientHeight"))?($("#gametable_lhs tr.iconlist").attr("clientHeight")):(parseInt($("#gametable_lhs tr.iconlist").css("height")));
            var rhsIconHeight = ($("#gametable_rhs tr.iconlist").attr("clientHeight"))?($("#gametable_rhs tr.iconlist").attr("clientHeight")):(parseInt($("#gametable_rhs tr.iconlist").css("height")));
            if(lhsIconHeight > rhsIconHeight)
            {
                ($("#gametable_lhs tr.iconlist td").css( "height", lhsIconHeight + "px" ));
                ($("#gametable_rhs tr.iconlist td").css( "height", lhsIconHeight + "px" ));
            }
            else
            {
                ($("#gametable_lhs tr.iconlist td").css( "height", rhsIconHeight + "px" ));
                ($("#gametable_rhs tr.iconlist td").css( "height", rhsIconHeight + "px" ));
            }
        }
        
		$("a#close-1").click(function() {
			$(".chat ul").tabsRemove(1);
			return false;		
		});
		
		
	}); //ready
	
	$(function() { $(".chat ul").tabs(); });
	$(function() { $(".result ul").tabs(); });
	
	
