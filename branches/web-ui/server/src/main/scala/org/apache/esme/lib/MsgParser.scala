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

package org.apache.esme.lib


import scala.util.parsing.combinator.{Parsers, ImplicitConversions}
import scala.util.matching._
import net.liftweb._
import util._
import mapper._
import Helpers._
import net.liftweb.util.{Failure => BoxFailure}
import scala.util.parsing.input.Reader
import scala.xml.{XML, NodeSeq, Node, Text}
import org.apache.esme._
import model._

object MsgParser extends Parsers with ImplicitConversions with CombParserHelpers {
  def parseMessage(in: String): Box[List[MsgInfo]] = begin(in) match {
    case Success(xs, _) => Full(xs)
    case _ => Empty
  }

  lazy val begin: Parser[List[MsgInfo]] = 
  startSpace ~> rep1(url | atName | hashTag | text) <~ spaceEOF

  lazy val startSpace = rep(' ')

  lazy val url: Parser[URL] = httpUrl ^^ {url => URL(UrlStore.make(url))}

  lazy val atNameStr: Parser[String] = alpha ~ rep(alpha | digit | '_') ^^ {
    case first ~ more => first + more.mkString
  }
  
  lazy val atName: Parser[MsgInfo] = '@' ~> atNameStr ~ rep('.' ~> atNameStr) ^^ {
    case name ~ domainlist => 
      val nickName: String = name
      val wholeName: String = (name :: domainlist).mkString(".")
      User.find(By(User.nickname, nickName)) match {
        case Full(u) => AtName(u)
        case _ => MsgText("@"+wholeName)
      }
  }
  
  // def ip_schemepart = (accept("//") ~> login) ~> opt( '/' ~> urlpath)

  lazy val login: Parser[String] = userPass ^^ {
    case ("", _) => ""
    case (user, "") => user + "@"
    case (user, password) => user + ":" + password + "@"
  }
  
  lazy val userPass: Parser[(String, String)] =
  opt(user ~ opt( ':' ~>  password ) <~ '@' ) ^^ {
    case None => ("", "")
    case Some(user ~ pwd) => (user, pwd.getOrElse(""))
  }

  lazy val hostport: Parser[String] = host ~ opt( ':' ~> port ) ^^ {
    case host ~ port => host + (port.map(p => ":"+p).getOrElse(""))
  }
  
  lazy val host: Parser[String] = hostname | hostnumber

  lazy val hostname: Parser[String] = rep( domainlabel <~ '.' ) ~ toplabel ^^ {
    case lst ~ end => (lst ::: List(end)).mkString(".")
  }

  lazy val domainlabel: Parser[String] = 
  (alphadigit ~ rep(alphadigit | '-') /*~ alphadigit */ ^^ {
      case d ~ dl /* ~ d3*/ => d + dl.mkString /*+ d3*/}) |
  (alphadigit ^^ {_.toString})

  lazy val toplabel: Parser[String] = 
  (alpha ~ rep(alphadigit | '-' ) /* ~ alphadigit */ ^^ {
      case a ~ al /* ~ a3*/ => a + al.mkString // + a3
    })  | (alpha ^^ {_.toString})

  lazy val alphadigit: Parser[Elem] = alpha | digit

  lazy val hostnumber: Parser[String] = 
  digits ~ '.' ~ digits ~ '.' ~ digits ~ '.' ~ digits ^^ {
    case one ~ _ ~ two ~ _ ~ three ~ _ ~ four =>
      one + "." + two + "." + three + "." + four
  }
  
  lazy val port: Parser[String] = digits 
  lazy val user: Parser[String] = rep( uchar | ';' | '?' | '&' | '=' ) ^^ {_.mkString}
  lazy val password: Parser[String] = user

  lazy val mailtoUrl: Parser[String] = accept("mailto:") ~> emailAddr

  lazy val emailAddr: Parser[String] = rep1(xchar) ^^ {
    case xs => xs.mkString
  }

  lazy val scheme: Parser[String] = (accept("http://") | accept("https://")) ^^ {_ mkString}

  lazy val httpUrl: Parser[String] = scheme ~ login ~ urlpart ^^ {
    case front ~ login ~ urlpart => front + login + urlpart
  }

  lazy val urlpart: Parser[String] = 
  hostport ~ opt( '/' ~> hpath ~ opt('?' ~> search )) ^^ {
    case hp ~ None => hp
    case hp ~ Some(pth ~ None) => hp + "/" + pth
    case hp ~ Some(pth ~ Some(search)) =>
      hp + "/" + pth + "?" + search
  }

  lazy val hpath: Parser[String] = hsegment ~ rep('/' ~> hsegment) ^^ {
    case x ~ xs => (x :: xs).mkString("/")
  }


  lazy val hsegment: Parser[String] = rep(uchar | ';' | ':' | '@' | '&' | '=') ^^
  {_.mkString}


  lazy val search: Parser[String] = rep(uchar | ';' | ':' | '@' | '&' | '=' | '/') ^^ {
    _.mkString
  }

  lazy val lowAlpha: Parser[Elem] = elem("Low Alpha", c => (c >= 'a' && c <= 'z'))

  lazy val hiAlpha: Parser[Elem] = elem("High Alpha", c => (c >= 'A' && c <= 'Z'))
  
  lazy val safe: Parser[Elem] = elem("Safe", c => c == '$' || c == '-' || c == '_' || c == '.' ||
                                     c == '+')
  lazy val reserved: Parser[Elem] = elem("Safe", c => c == ';' || c == '/' ||
                                         c == '?' || c == ':' ||
                                         c == '@' ||
                                         c == '&' ||  c == '=')

  lazy val extra: Parser[Elem] = elem("Extra", c => c == '!' || c == '*' || c == '\'' ||
                                      c == '(' || c == ')' || c == ',')

  lazy val hex: Parser[Elem] = elem("Hex", c => (c >= '0' && c <= '9') ||
                                    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

  lazy val escape: Parser[Elem] = '%' ~> hex ~ hex ^^ {
    case high ~ low => Integer.parseInt(high.toString + low.toString, 16).toChar
  }
  
  lazy val alpha: Parser[Elem] = lowAlpha | hiAlpha
  lazy val unreserved: Parser[Elem] = alpha | digit | safe | extra
  lazy val uchar: Parser[Elem] = unreserved | escape
  lazy val xchar: Parser[Elem] = unreserved | reserved | escape

  lazy val digits: Parser[String] = rep1(digit) ^^ {_.mkString}
  
  lazy val nameChar: Parser[Elem] = elem("Name Char", isNameChar _)
  
  def isNameChar(in: Char): Boolean = isTagChar(in) 

  lazy val hashTag: Parser[HashTag] = '#' ~> rep1(tagChar) ^^ {
    case xs => HashTag(Tag.findOrCreate(xs))
  }
  
  lazy val tagChar: Parser[Elem] = elem("Tag Char", isTagChar _)
  
  def isTagChar(in: Char): Boolean = Character.isLetter(in) ||
  Character.isDigit(in) || (in == '_')

  lazy val spaceEOF = rep(' ') ~ EOF

  lazy val text: Parser[MsgText] = rep1(not(spaceEOF) ~ not(url) ~ not(atName) ~
                                        not(hashTag) ~> anyChar) ^^ {
    case xs => MsgText(xs.mkString(""))
  }

  lazy val EOF: Parser[Elem] = elem("EOF", isEof _)

  def perform(in: String): Box[Performances] = _perform(in) match {
    case Success(p, _) => Full(p)
    case _ => Empty
  }

  lazy val _perform: Parser[Performances] =
  (acceptCI("filter") ~ lineSpace ~ EOF ^^^ PerformFilter) |
  (acceptCI("resend") ~ lineSpace ~ EOF ^^^ PerformResend) |
  (mailtoUrl ~ opt(rep(EOL) ~> rep1(anyChar)) <~ EOF ^^ {
    case mt ~ text => MailTo(mt, text.map(_ mkString))
  }) |
  (scheme ~ userPass ~ urlpart ~ rep(httpHeader) ~ httpData <~ EOF ^^ {
      case protocol ~ userPass ~ urlpart ~ hdrs ~ data =>
        HttpTo(protocol + urlpart, userPass._1, userPass._2, hdrs, data)
    }) |
  (acceptCI("atom:") ~> httpUrl <~ EOF ^^ {url => FetchAtom(UrlStore.make(url))}) |
  (acceptCI("rss:") ~> httpUrl <~ EOF ^^ {url => FetchRss(UrlStore.make(url))})

  lazy val httpHeader: Parser[(String, String)] = EOL ~ accept("header:") ~
  lineSpace ~> rep1(uchar) ~ '=' ~ rep1(uchar) ^^ {
    case name ~ _ ~ value => (name.mkString, value.mkString)
  }

  lazy val httpData: Parser[Option[String]] = opt(EOL ~> rep1(anyChar)) ^^ { _ map(_ mkString) }

  def testMessage(in: String): Box[TestAction] = _testMessage(in) match {
    case Success(ta, _) => Full(ta)
    case _ => Empty
  }
  
  lazy val _testMessage: Parser[TestAction] = testExpr

  lazy val testExpr: Parser[TestAction] = phrase(_testExpr)

  lazy val _testExpr: Parser[TestAction] = 
  testFactor*(orOp ^^^ {(l, r) => OrAction(l, r)} | andOp ^^^ {(l,r) => AndAction(l, r)})

  lazy val orOp: Parser[String] = whiteSpace ~ '|' ~ whiteSpace ^^^ "|"

    lazy val andOp: Parser[String] = whiteSpace ~ '&' ~ whiteSpace ^^^ "&"
  
  lazy val testFactor: Parser[TestAction] = (notTest |
  testAt | testRegex | testString |
  testTag | 
  testParen | testPercent |
  testDates | testLogin |
  testFollowed | testUnfollowed |
  testProfile | testRegular |
  anyMsg | testToMe) <~ whiteSpace

  lazy val toOpr: Parser[EqOprType] =
  ('=' ^^^ EqOpr) | (accept("<>") ^^^ NeOpr)

  lazy val anyOpr: Parser[OprType] =
  accept(">=") ^^^ GeOpr |
  accept("<=") ^^^ LeOpr |
  accept("<>") ^^^ NeOpr |
  accept("<") ^^^ LtOpr |
  accept(">") ^^^ GtOpr |
  accept("=") ^^^ EqOpr

  lazy val dateKeyword: Parser[DateType] = 
  acceptCI("day") ^^^ DayDateType | 
  acceptCI("date") ^^^ DateDateType |
  acceptCI("hour") ^^^ HourDateType  |
  acceptCI("month") ^^^ MonthDateType | 
  acceptCI("minute") ^^^ MinuteDateType

  lazy val number: Parser[Int] = rep1(digit) ^^ {case x => x.mkString.toInt}

  lazy val numberList: Parser[List[Int]] =
  whiteSpace ~ '(' ~ whiteSpace ~> number ~ rep(whiteSpace ~ ',' ~ whiteSpace ~> number) <~
  whiteSpace ~ ')' ~ whiteSpace ^^ {
    case x ~ xs => x :: xs
  }

  lazy val testLogin: Parser[TestAction] = acceptCI("login") ^^^ LoginAction

  lazy val testFollowed: Parser[TestAction] = acceptCI("followed") ^^^ FollowedAction

  lazy val testUnfollowed: Parser[TestAction] = acceptCI("unfollowed") ^^^ UnfollowedAction

  lazy val testProfile: Parser[TestAction] = acceptCI("profile") ^^^ ProfileAction

  lazy val testRegular: Parser[TestAction] = acceptCI("every") ~ whiteSpace ~> number <~ whiteSpace ~ acceptCI("mins") ^^ {
    case mins => RegularAction(mins)
  }

  lazy val testDates: Parser[TestAction] =
  ((whiteSpace ~> dateKeyword) ~ (whiteSpace ~> toOpr) ~ (whiteSpace ~> numberList) ^^
   {
      case kw ~ opr ~ lst => DateTestAction(kw, opr, lst)
    }) | ((whiteSpace ~> dateKeyword) ~ (whiteSpace ~> anyOpr) ~ (whiteSpace ~> number) ^^
          {
      case kw ~ opr ~ num => DateTestAction(kw, opr, List(num))
    })


  lazy val toPeople: Parser[List[AtUserAction]] = 
  (whiteSpace ~ '(' ~ whiteSpace ~> testAt ~ 
   rep(whiteSpace ~ ',' ~ whiteSpace ~> testAt) <~ whiteSpace ~ ')' ~ whiteSpace ^^
   {
      case f ~ lst => f :: lst
    }
  ) | testAt ^^ {case ta => List(ta)}

  lazy val testTo: Parser[TestAction] =
  whiteSpace ~ acceptCI("to") ~ whiteSpace ~> toOpr ~ whiteSpace ~ toPeople ^^ {
    case opr ~ _ ~ who => AtSendAction(who.map(_.userId), opr)
  }

  lazy val testPercent: Parser[TestAction] =
  whiteSpace ~> digit ~ opt(digit) <~ '%' ~ whiteSpace ^^ {
    case d ~ d2 => val str = d.toString + (d2.map(_.toString).getOrElse(""))
      PercentAction(str.toInt)
  }

  lazy val testParen: Parser[TestAction] =
  whiteSpace ~ '(' ~ whiteSpace ~> _testExpr <~ whiteSpace ~ ')' ~ whiteSpace ^^ {
    case x => ParenAction(x)
  }

  lazy val testAt: Parser[AtUserAction] =
  (whiteSpace ~ '@' ~> rep1(digit) <~ whiteSpace ^^ {case dig => AtUserAction(dig.mkString.toLong)}) |
  (atName ^^ {
      case AtName(user) => AtUserAction(user.id)
    })

  lazy val reChars: Parser[Char] = (('\\' ~ '/') ^^^ '/') |
  (not('/') ~> anyChar)
  
  lazy val testRegex: Parser[TestAction] = 
  whiteSpace ~ '/' ~> rep1(reChars) <~ '/' ~ whiteSpace ^^ {
    case re if validRegex(re.mkString).isDefined => RegexAction(re.mkString)
  }
  
  lazy val strChars: Parser[Char] = (('\\' ~ '"') ^^^ '/') |
  (not('"') ~> anyChar)
  
  lazy val testString: Parser[TestAction] =
  whiteSpace ~ '"' ~> rep1(strChars) <~ '"' ~ whiteSpace ^^ {
    case re => StringAction(re.mkString)
  }

  def validRegex(in: String): Box[Regex] = tryo(in.r)

  lazy val testTag: Parser[TestAction] = whiteSpace ~ '#' ~> rep1(tagChar) <~ whiteSpace ^^ {
    case xs => HashAction(Tag.findOrCreate(xs.mkString).id, xs.mkString)
  }

  lazy val testOr: Parser[TestAction] = 
  (testExpr <~ whiteSpace ~ '|' ~ whiteSpace) ~ testExpr ^^ {
    case left ~ right => OrAction(left, right)
  }

  lazy val testAnd: Parser[TestAction] = 
  (testExpr <~ whiteSpace ~ '&' ~ whiteSpace) ~ testExpr ^^ {
    case left ~ right => AndAction(left, right)
  }
  
  lazy val notTest: Parser[TestAction] = whiteSpace ~ acceptCI("not(") ~> testExpr <~ whiteSpace ~ ')' ^^ {
    case x => NotAction(x)
  }
  
  lazy val anyMsg: Parser[TestAction] = 
  whiteSpace ~ acceptCI("any") ~ whiteSpace ^^^ AnyAction
  
  lazy val testToMe: Parser[TestAction] = whiteSpace ~ acceptCI("tome") ~ whiteSpace ^^^ SentToMeAction

}

sealed trait MsgInfo
case class MsgText(text: String) extends MsgInfo
case class AtName(user: User) extends MsgInfo
case class HashTag(tag: Tag) extends MsgInfo
case class URL(url: UrlStore) extends MsgInfo
