package org.apache.esme.model

/**
 * Copyright 2008-2009 WorldWide Conferencing, LLC
 * 
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import net.liftweb._
import mapper._
import http._
import util._

import org.apache.esme._
import lib._
import actor._
import external._

import java.util.Calendar
import scala.xml.{Text, Node, Elem => XmlElem}

object Action extends Action with LongKeyedMetaMapper[Action] {
  override def afterCommit = notifyDistributor _ :: super.afterCommit

  private def notifyDistributor(in: Action) {
    Distributor ! Distributor.UpdateTrackingFor(in.user, 
                                                Distributor.PerformTrackingType)
  }

  override def afterSave = startStopActors _ :: super.afterSave
  
  private def startStopActors(in: Action)  {
    if (!in.removed.is && in.enabled) {
      in.startActors()
    } else {
      SchedulerActor ! SchedulerActor.StopRegular(in.id)
      MessagePullActor ! MessagePullActor.StopPullActor(in.id)
    }
  }
  
  type TestFunc = (Message, Long, Calendar, MailboxReason) => Boolean
  
  lazy val TrueFunc: TestFunc = {case _ => true}
  lazy val SentToMe: TestFunc = (m, u, c, r) => m.sentToIds.contains(u)
  
  def toFunc(in: TestAction): TestFunc = in match {
    case AnyAction => TrueFunc

    case NotAction(action) =>
      val f: TestFunc = this.toFunc(action)
      (m, u, c, r) => !f(m,u,c,r)

    case OrAction(left, right) =>
      val f1 = toFunc(left)
      val f2 = toFunc(right)
      (m, u, c, r) => f1(m, u, c, r) || f2(m, u, c, r)
  
    case AndAction(left, right) =>
      val f1 = toFunc(left)
      val f2 = toFunc(right)
      (m, u, c, r) => f1(m, u, c, r) && f2(m, u, c, r)
  
    case LoginAction() =>
      (m, u, c, r) => r.isInstanceOf[LoginReason]

    case FollowedAction() =>
      (m, u, c, r) => r.isInstanceOf[FollowedReason]

    case UnfollowedAction() =>
      (m, u, c, r) => r.isInstanceOf[UnfollowedReason]
      
    case RegularAction(mins) =>
      (m, u, c, r) => r.isInstanceOf[RegularReason]
      
    case ProfileAction() =>
      (m, u, c, r) => r.isInstanceOf[ProfileReason]
      
    case AtUserAction(userId) =>
      (m, u, c, r) => m.author.is == userId 
        
    case SentToMeAction =>
      SentToMe
      
    case RegexAction(re) =>
      val r = re.r
      (m, u, c, reason) => r.findFirstIn(m.getText).isDefined
        
    case StringAction(s) =>
      val str = s.toLowerCase.trim
      (m, u, c, r) => m.getText.toLowerCase.indexOf(str) >= 0
        
    case HashAction(id, _) =>
      (m, u, c, r) => m.tagIds.contains(id)
        
    case ParenAction(a) =>
      toFunc(a)
        
    case PercentAction(percent) =>
      (m, u, c, r) => Helpers.randomInt(100) <= percent
      
    case  AtSendAction(users, EqOpr) =>
      (m, u, c, r) => !m.sentToIds.intersect(users).isEmpty

    case  AtSendAction(users, NeOpr) =>
      (m, u, c, r) => m.sentToIds.intersect(users).isEmpty
      
    case DateTestAction(dt, ot, what) =>
      (m, u, c, r) => ot.buildFunc(dt.buildFunc(c), what)
  }
  
  def regularActions(in: TestAction): List[RegularAction] = in match {
    case NotAction(a) => regularActions(a)

    case ParenAction(a) => regularActions(a)
    
    case OrAction(left, right) => regularActions(left) ::: regularActions(right)
  
    case AndAction(left, right) => regularActions(left) ::: regularActions(right)

    case a @ RegularAction(mins) => List(a)
        
    case _ => Nil
  }
}

class Action extends LongKeyedMapper[Action] {

  def startActors() {
    for(regular <- regularActions) regular match { 
      case RegularAction(mins) => SchedulerActor ! SchedulerActor.StartRegular(this, mins * 60)
    }
    val urlSourcePrefix = "url:"
    theAction.actionFunc match {
      case a @ (FetchFeed(url)) => {
        User.find(user) match {
          case Full(u) =>
            val msgList = Message.findAll(By(Message.source, urlSourcePrefix + url.uniqueId),
                                          OrderBy(Message.id, Descending),
                                          MaxRows(1))
            val lastMsg = if (msgList.isEmpty) None 
              else {
                val m = msgList.first
                Some(Distributor.UserCreatedMessage(user, m.getText, m.tags, m.when, Empty, m.source, Full(m.replyTo)))
              }

            val feed = a match {
              case FetchAtom(_) => new AtomFeed(u, url.url, urlSourcePrefix + url.uniqueId, 0, Nil)
              case FetchRss(_) => new RssFeed(u, url.url, urlSourcePrefix + url.uniqueId, 0, Nil)
            }
            MessagePullActor ! MessagePullActor.StartPullActor(id, lastMsg, feed)
        }
      }
      case _ =>
    }
  }

  def getSingleton = Action // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)

  object user extends MappedLongForeignKey(this, User)
  object name extends MappedPoliteString(this, 64) {
    
    override def validations =
    valMinLen(2, "The name must be at least 2 characters long") _ :: super.validations
    
  }
  private[model] object theAction extends MappedText(this) {
    import MsgParser._
    
    override def validations = checkParsing _ :: super.validations
    def checkParsing(in: String): List[FieldError] = _perform(in) match {
      case Failure(msg, _) => List(FieldError(this, Text(msg)))
      case Error(msg, _) => List(FieldError(this, Text(msg)))
      case _ => Nil
    }
    
    def actionFunc = _perform(is) match {
      case Success(v, _) => v
    }
  }
  
  private[model] object theTest extends MappedText(this) {
    import MsgParser._
    
    override def validations = checkParsing _ :: super.validations
    def checkParsing(in: String): List[FieldError] = testExpr(in) match {
      case Failure(msg, _) => List(FieldError(this, Text(msg)))
      case Error(msg, _) => List(FieldError(this, Text(msg)))
      case _ => Nil
    }

    def testFunc = testExpr(is) match {
      case Success(v, _) => Action.toFunc(v)
    }
  }
  
  object uniqueId extends MappedUniqueId(this, 32) {
    override def dbIndexed_? = true
  }
  object removed extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object disabled extends MappedBoolean(this) {
    override def defaultValue = false
  }
  import MsgParser._

  def setTest(in: String): Box[Action] = testExpr(in) match {
    case Success(v, _) => Full(this.theTest(v.toStr))
    case Failure(m, _) => net.liftweb.util.Failure(m, Empty, Empty)
    case Error(m, _) => net.liftweb.util.Failure(m, Empty, Empty)
  }

  def testText = theTest.is

  def regularActions: List[RegularAction] = testExpr(testText) match {
    case Success(v, _) => Action.regularActions(v)
  }

  def actionText = theAction.is
  
  def setAction(in: String): Box[Action] = _perform(in) match {
    case Success(_, _) => Full(this.theAction(in))
    case Failure(m, _) => net.liftweb.util.Failure(m, Empty, Empty)
    case Error(m, _) => net.liftweb.util.Failure(m, Empty, Empty)
  }



  def matcher: PerformMatcher = {
    new PerformMatcher(theTest.testFunc, id, uniqueId,
                       theAction.actionFunc)
  }

  def enabled: Boolean = !disabled.is

  override def toXml: XmlElem =
  <action id={id.toString}
    name={name.is}
    test={theTest.is}
    action={theAction.is}
    enabled={enabled.toString}></action>

}

class PerformMatcher(val func: Action.TestFunc, val performId: Long,
                     val uniqueId: String, val whatToDo: Performances) {
  def doesMatch(msg: Message, userId: Long, cal: Calendar, reason: MailboxReason): Boolean =
  func(msg, userId, cal, reason)

  def filter_? = whatToDo == PerformFilter
}



sealed trait TestAction {
  def toStr: String
}
case object AnyAction extends TestAction {
  def toStr = "any"
}
case object SentToMeAction extends TestAction {
  def toStr = "tome"
}
case class NotAction(action: TestAction) extends TestAction {
  def toStr = "not( "+action.toStr+" )"
}
case class OrAction(left: TestAction, right: TestAction) extends TestAction {
  def toStr = left.toStr + " | " + right.toStr
}

case class AndAction(left: TestAction, right: TestAction) extends TestAction {
  def toStr = left.toStr + " &  " + right.toStr
}

case class AtUserAction(userId: Long) extends TestAction {
  def toStr = "@"+userId
}

case class RegexAction(re: String) extends TestAction {
  def toStr = "/"+fix+"/"
  
  def fix: String = {
    val ret = new StringBuilder
    var pos = 0
    val len = re.length
    while (pos < len) {
      re.charAt(pos) match {
        case '/' => ret.append("\\/")
        case c => ret.append(c)
      }
      pos += 1
    }
    ret.toString
  }
}

case class StringAction(re: String) extends TestAction {
  def toStr = "\""+fix+"\""
  
  def fix: String = {
    val ret = new StringBuilder
    var pos = 0
    val len = re.length
    while (pos < len) {
      re.charAt(pos) match {
        case '"' => ret.append("\\\"")
        case c => ret.append(c)
      }
      pos += 1
    }
    ret.toString
  }
}

case class HashAction(hashId: Long, str: String) extends TestAction {
  def toStr = "#"+str
}

case class ParenAction(action: TestAction) extends TestAction {
  def toStr = "( "+action.toStr+" )"
}

case class PercentAction(percent: Int) extends TestAction {
  def toStr = percent+"% "
}

case class AtSendAction(users: List[Long], opr: EqOprType) extends TestAction {
  def toStr = "to "+opr.toStr+" "+(users match {
      case x :: Nil => "@"+x
      case xs => xs.map(v => "@"+v).mkString("(", ", ", ")")
    })
}

case class LoginAction extends TestAction {
  def toStr = "login"
}

case class FollowedAction extends TestAction {
  def toStr = "followed"
}

case class UnfollowedAction extends TestAction {
  def toStr = "unfollowed"
}

case class ProfileAction extends TestAction {
  def toStr = "profile"
}

case class RegularAction(mins: Int) extends TestAction {
  def toStr = "every " + mins + " mins"
}

case class DateTestAction(dateType: DateType, opt: OprType, what: List[Int]) extends TestAction {
  def toStr = dateType.toStr + " " + opt.toStr + " " + (
    what match {
      case x :: Nil => x.toString
      case xs => xs.mkString("(", ", ", ")")
    }
  )
}

sealed trait OprType {
  def buildFunc: (Int, List[Int]) => Boolean
  def toStr: String
}

sealed trait EqOprType extends OprType
case object EqOpr extends EqOprType {
  val buildFunc: (Int, List[Int]) => Boolean = (v, l) => l.forall(_ == v)
  def toStr: String = "="
}
case object NeOpr extends EqOprType {
  val buildFunc: (Int, List[Int]) => Boolean = (v, l) => l.forall(_ != v)
  def toStr: String = "<>"
}
case object GeOpr extends OprType {
  val buildFunc: (Int, List[Int]) => Boolean = (v, l) => l.forall(_ >= v)
  def toStr: String = ">="
}
case object GtOpr extends OprType {
  val buildFunc: (Int, List[Int]) => Boolean = (v, l) => l.forall(_ > v)
  def toStr: String = ">"
}
case object LeOpr extends OprType {
  val buildFunc: (Int, List[Int]) => Boolean = (v, l) => l.forall(_ <= v)
  def toStr: String = "<="
}
case object LtOpr extends OprType {
  val buildFunc: (Int, List[Int]) => Boolean = (v, l) => l.forall(_ < v)
  def toStr: String = "<"
}

sealed trait DateType {
  def buildFunc: Calendar => Int
  def toStr: String
}
case object DayDateType extends DateType {
  val buildFunc: Calendar => Int = c => c.get(Calendar.DAY_OF_WEEK)
  def toStr: String = "day"
}
case object DateDateType extends DateType {
  val buildFunc: Calendar => Int = c => c.get(Calendar.DAY_OF_MONTH)
  def toStr: String = "date"
}
case object MonthDateType extends DateType {
  val buildFunc: Calendar => Int = c => c.get(Calendar.MONTH)
  def toStr: String = "month"
}
case object HourDateType extends DateType {
  val buildFunc: Calendar => Int = c => c.get(Calendar.HOUR_OF_DAY)
  def toStr: String = "hour"
}
case object MinuteDateType extends DateType {
  val buildFunc: Calendar => Int = c => c.get(Calendar.MINUTE)
  def toStr: String = "minute"
}

sealed trait Performances
case class MailTo(who: String, text: Option[String]) extends Performances
case class HttpTo(url: String, user: String, password: String, headers: List[(String, String)], data: Option[String]) extends Performances
case class FetchFeed(url: UrlStore) extends Performances
case class FetchAtom(override val url: UrlStore) extends FetchFeed(url)
case class FetchRss(override val url: UrlStore) extends FetchFeed(url)
case object PerformResend extends Performances
case object PerformFilter extends Performances
