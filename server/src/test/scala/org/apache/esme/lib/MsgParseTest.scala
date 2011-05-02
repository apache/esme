/**
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
/*
import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import net.liftweb.common._
import org.specs.matcher._
import Helpers._
import MsgParser._                                      
import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.{Context, FilterHolder}
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.webapp.WebAppContext
import org.apache.esme._
import model._
import net.liftweb.http._
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework}
                                                       
import _root_.junit.framework.AssertionFailedError

class MsgParserSpecsAsTest extends JUnit3(MsgParserSpecs)
object MsgParserSpecsRunner extends ConsoleRunner(MsgParserSpecs)

object MsgParserSpecs extends Specification with TestKit { 
  JettyTestServer.start 

  val baseUrl = JettyTestServer.urlFor("")

  implicit val reportError = new ReportFailure {
    def fail(msg: String): Nothing = MsgParserSpecs.this.fail(msg)
  }
  
    def shouldnt(f: => Unit): Unit =
    try {
      val x = f
      fail("Shouldn't succeed")
    } catch {
      case _ => ()
    }
   
  type PFT = MsgParser.ParseResult[_]
  def parseMatch(name: String, matchr: PartialFunction[PFT, Any]) = new Matcher[PFT] {
    def apply(v: => PFT) = (matchr.isDefinedAt(v),
                            name+" succeeded parsing",
                            name+" failed parsing")
  }

  "Msg Parser Parse" should {
    "parse top label" in {
      val ret = MsgParser.toplabel("www")

      ret must
      parseMatch("www", {
          case MsgParser.Success("www", _) =>
        })
    }

    "parse a simple URL" in {
      MsgParser.httpUrl("http://www.google.com") must
      parseMatch("google", {
          case MsgParser.Success("http://www.google.com", _) =>
        })
    }

    "parse a complex simple URL" in {
      val Str = "http://www.google.com:8080/Foo/bar"

      val ret = MsgParser.httpUrl(Str)

      ret must
      parseMatch("google + port", {
          case MsgParser.Success(Str, _) =>
        })
    }

    "parse a complex URL with query params" in {
      val Str = "https://www.sdn.sap.com/irj/sdn/weblogs?blog=/pub/wlg/10754"

      val ret = MsgParser.httpUrl(Str)

      ret must
      parseMatch("sap thing", {
          case MsgParser.Success(Str, _) =>
        })
    }

    "Fail for an empty string" in {
      MsgParser.begin("") must
      parseMatch("Empty",
                 {case MsgParser.Error(_, _) =>
          case MsgParser.Failure(_, _) =>
        })

    }

    "Find only text" in {
      MsgParser.begin("This is a message") must
      parseMatch("only test",
                 {case MsgParser.Success(MsgText(_) :: Nil, _) =>

        })

    }

    "Find only text" in {
      MsgParser.begin("This is http://$$$ a message") must
      parseMatch("only test",
                 {case MsgParser.Success(MsgText(_) :: Nil, _) =>

        })

    }

    "Find only text" in {
      MsgParser.begin("This is #,hello a message") must
      parseMatch("only test",
                 {case MsgParser.Success(MsgText(_) :: Nil, _) =>

        })

    }

    "Find only text" in {
      MsgParser.begin("This is @--Moose a message") must
      parseMatch("only test",
                 {case MsgParser.Success(MsgText(_) :: Nil, _) =>

        })

    }

    "Find a hash tag" in {
      MsgParser.begin("This is #hash a message") must
      parseMatch("only test",
                 {case MsgParser.Success(MsgText(_) ::
                                         HashTag(tag) ::
                                         MsgText(_) :: Nil, _)
            if tag.name.equalsIgnoreCase("hash") =>
        }
      )

    }

    "Find a @msg tag" in {
      val ret = MsgParser.begin("This is @hash, a message")

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(MsgText(_) ::
                                         AtName(_) ::
                                         MsgText(_) :: Nil, _) =>
        }
      )

    }

    "Find a url tag" in {
      MsgParser.begin("This is http://www.moo.com a message") must
      parseMatch("only test",
                 {case MsgParser.Success(MsgText(_) ::
                                         URL(_) ::
                                         MsgText(_) :: Nil, _) =>
        }
      )

    }

    "Find a https url tag" in {
      MsgParser.begin("This is https://www.moo.com a message") must
      parseMatch("only test",
                 {case MsgParser.Success(MsgText(_) ::
                                         URL(_) ::
                                         MsgText(_) :: Nil, _) =>
        }
      )

    }

    "match 'any'" in {
      val ret =  MsgParser._testMessage("any")

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(AnyAction, _) =>
        }
      )

    }


    "match '54%'" in {
      val ret =  MsgParser._testMessage(" 54% ")

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(PercentAction(54), _) =>
        }
      )

    }

    "match '#foo'" in {
      val ret =  MsgParser._testMessage(" #foo ")

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(HashAction(_, _), _) =>
        }
      )

    }

    /*

    "match '@hash'" in {
      println("About to test @hash")
      val ret =  MsgParser._testMessage(" @hash ")

      println("hash is is "+ret)

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(AtUserAction(_), _) =>
        }
      )

    }
    */

    "match '@hasher'" in {
      val ret =  MsgParser._testMessage(" @hasher ")

      ret must
      parseMatch(" @hasher ",
                 {case MsgParser.Failure(_ , _) =>
        }
      )

    }

    "match 'day = 4'" in {
      val ret =  MsgParser._testMessage(" day = 4 ")

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(DateTestAction(DayDateType, EqOpr, List(4)), _) =>
        }
      )

    }

    "match 'month = (1,2,4)'" in {
      val ret =  MsgParser._testMessage(" month = (1, 2,     4   ) ")

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(DateTestAction(MonthDateType, EqOpr, List(1, 2, 4)), _) =>
        }
      )

    }

    "match 'month = (1,2,4) | 74%'" in {
      val ret =  MsgParser._testMessage(" month = (1, 2,     4   )|74% ")

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(OrAction(DateTestAction(MonthDateType, EqOpr, List(1, 2, 4)), PercentAction(74)), _) =>
        }
      )

    }

    "match '( month = (1, 2,     4   )|74%) & #frog'" in {
      val ret =  MsgParser._testMessage("( month = (1, 2,     4   )|74%) & #frog ")

      ret must
      parseMatch("only test",
                 {case MsgParser.Success(AndAction(
                ParenAction(OrAction(DateTestAction(MonthDateType, EqOpr, List(1, 2, 4)), PercentAction(74))),
                  HashAction(_, _)), _) =>
        })

    }
  
  }
  

}*/
