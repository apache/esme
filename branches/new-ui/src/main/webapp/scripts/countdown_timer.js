c1 = new Image(); c1.src = liftContextPath+"/images/clock/c1.gif";
c2 = new Image(); c2.src = liftContextPath+"/images/clock/c2.gif";
c3 = new Image(); c3.src = liftContextPath+"/images/clock/c3.gif";
c4 = new Image(); c4.src = liftContextPath+"/images/clock/c4.gif";
c5 = new Image(); c5.src = liftContextPath+"/images/clock/c5.gif";
c6 = new Image(); c6.src = liftContextPath+"/images/clock/c6.gif";
c7 = new Image(); c7.src = liftContextPath+"/images/clock/c7.gif";
c8 = new Image(); c8.src = liftContextPath+"/images/clock/c8.gif";
c9 = new Image(); c9.src = liftContextPath+"/images/clock/c9.gif";
c0 = new Image(); c0.src = liftContextPath+"/images/clock/c0.gif";
cb = new Image(); cb.src = liftContextPath+"/images/clock/cb.gif";

function CountdownTimer( objId, hours, mins, secs )
{
    this.myId = objId;
    this.hoursLeft = hours;
    this.minsLeft = mins;
    this.secsLeft = secs;
}
CountdownTimer.prototype.updateTimeLeft = function()
{
    var countdownImageElems = document.getElementById( this.myId ).getElementsByTagName("img");
    if (!countdownImageElems) return;
    if (this.hoursLeft <= 9) 
    {
        countdownImageElems[0].src = cb.src;
        countdownImageElems[1].src = eval("c"+this.hoursLeft+".src");
    }
    else 
    {
        countdownImageElems[0].src = eval("c"+Math.floor(this.hoursLeft/10)+".src");
        countdownImageElems[1].src = eval("c"+(this.hoursLeft%10)+".src");
    }
    if (this.minsLeft <= 9) 
    {
        countdownImageElems[3].src = c0.src;
        countdownImageElems[4].src = eval("c"+this.minsLeft+".src");
    }
    else 
    {
        countdownImageElems[3].src = eval("c"+Math.floor(this.minsLeft/10)+".src");
        countdownImageElems[4].src = eval("c"+(this.minsLeft%10)+".src");
    }
    if (this.secsLeft <= 9) 
    {
        countdownImageElems[6].src = c0.src;
        countdownImageElems[7].src = eval("c"+this.secsLeft+".src");
    }
    else 
    {
        countdownImageElems[6].src = eval("c"+Math.floor(this.secsLeft/10)+".src");
        countdownImageElems[7].src = eval("c"+(this.secsLeft%10)+".src");
    }
}
CountdownTimer.prototype.countdownTick = function ()
{
    this.secsLeft--;
    if(this.secsLeft == -1) 
    {
        this.secsLeft = 59;
        this.minsLeft--;
    }
    if(this.minsLeft == -1) 
    {
        this.minsLeft = 59;
        this.hoursLeft--;
    }
        
    this.updateTimeLeft();
    if((this.hoursLeft == 0) && (this.minsLeft == 0) && (this.secsLeft == 0)) 
    {
        window.alert("Time is up. Press OK to continue."); // change timeout message as required
        // window.location = "yourpage.htm" // redirects to specified page once timer ends and ok button is pressed
    }
    else
    {
        setTimeout("CountdownTimer."+this.myId+".countdownTick()",1000);
    }
}
CountdownTimer.prototype.start = function()
{
    this.countdownTick();
}

function CreateCountdownTimer( objId, hours, mins, secs ) 
{
    if ( hours > 99 ) hours = 99;
    if ( mins > 59 ) mins = 59;
    if ( secs > 59 ) secs = 59;
    
    var newTimer = new CountdownTimer( objId, hours, mins, secs + 1 );
    CountdownTimer[ objId ] = newTimer;
    newTimer.render();
    newTimer.start();
    return newTimer;
}

CountdownTimer.prototype.render = function()
{
    var countdownTimerElem = document.getElementById( this.myId );
    
    countdownTimerElem.innerHTML = "<img src='"+liftContextPath+"/images/clock/cb.gif'/>" +
                                    "<img src='"+liftContextPath+"/images/clock/cb.gif'/>" +
                                    "<img src='"+liftContextPath+"/images/clock/colon.gif'/>" +
                                    "<img src='"+liftContextPath+"/images/clock/cb.gif'/>" +
                                    "<img src='"+liftContextPath+"/images/clock/cb.gif'/>" +
                                    "<img src='"+liftContextPath+"/images/clock/colon.gif'/>" +
                                    "<img src='"+liftContextPath+"/images/clock/cb.gif'/>" +
                                    "<img src='"+liftContextPath+"/images/clock/cb.gif'/>";
}
