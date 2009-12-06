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

package org.apache.esme.api 

import org.specs._
import org.specs.runner.JUnit3
import org.specs.runner.ConsoleRunner
import net.liftweb.util._
import net.liftweb.common._ 
import org.specs.matcher._
import Helpers._
import net.sourceforge.jwebunit.junit.WebTester
import org.mortbay.jetty.Server
import org.mortbay.jetty.servlet.{Context, FilterHolder}
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.webapp.WebAppContext
import org.apache.esme._
import model._
import net.liftweb.http._
import testing.{ReportFailure, TestKit, HttpResponse, TestFramework}  

import net.sourceforge.jwebunit.junit.WebTester
import _root_.junit.framework.AssertionFailedError                 

class Api2SpecsAsTest extends JUnit3(Api2Specs)
object Api2SpecsRunner extends ConsoleRunner(Api2Specs)

object Api2Specs extends Specification with TestKit {
  JettyTestServer.start

  val baseUrl = JettyTestServer.urlFor("/api2/")
  
  val session = new LiftSession(Helpers.randomString(20), "", Empty)

  val theUser = S.initIfUninitted(session) {User.createAndPopulate.nickname("api_test").saveMe}
  val token = {
    val toke = AuthToken.create.user(theUser).saveMe
    toke.uniqueId.is
  }     

  val post_session = post("session", "token" -> token)
  
  "API2" should {
	"/session POST" in {  
	  "Attempt to log in with a valid token should succeed with a 200 response" in {
	    for{
	      session <- post_session    
	    } {
	      (session.xml \ "session" \ "user" \ "id").text must be equalTo(theUser.id.toString)
		  session.code must be equalTo 200
	    } 
	  }

	  "Attempt to create session with an invalid token returns 403 response" in {
	    for{
	      session <- post("session", "token" -> "000000")
	    } {                  
          session.code must be equalTo 403
	    } 
	  }
	}
	
	"/session GET" in {
	  "with valid session" in {
	    for {
	      session <- post_session
          session_response <- session.get("session")
	    } {
	      session_response.code must be equalTo 200
	      ( session_response.xml \ "session" \ "user" \ "id").text must be equalTo(theUser.id.toString)
	    }
	  }
	
	  "with no session returns 404 (not found)" in {
	    for(session_res <- get("session")) {
	      session_res.code must be equalTo 404
	    }
	  }  
    }

    "/session DELETE" in {
      "with valid session" in {
        for {
          session <- post_session
          session_del_response <- session.delete("session")
          //session_response <- session.get("session")
        } {
          session_del_response.code must be equalTo 200
          //session_response.code must be equalTo(404)
        }
      }

	  "with no session returns 404 (not found)" in {
	    for(session_res <- delete("session")) {
	      session_res.code must be equalTo 404
	    }
	  }
    }	

    "/users GET" in {
      "with valid session" in {     
        for {
          session <- post_session
          users <- session.get("users")
        } {
          users.code must be equalTo 200
        }
      }

	  "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("users")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/messages GET" in {
      "with valid session" in {
        for {
          session <- post_session
          mess_res <- session.get("user/messages")
        } {
          mess_res.code must be equalTo 200
        }
      }

	  "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/messages")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/messages?history=10 GET" in {
      "with valid session" in {
        for {
          session <- post_session
          res <- session.get("user/messages?history=10")
        } {
          res.code must be equalTo 200
        }                     
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/messages?history=10")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/messages?timeout=2 GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("user/messages?timeout=2")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/messages?timeout=2")) {
	      session_res.code must be equalTo 403
	    }
	  }
    } 

    "/user/messages POST" in {
      "with valid session" in {
        for {
          session <- post_session
          mess_res <- session.post("user/messages","message"->"test message")
        } {
          mess_res.code must be equalTo 200
        }
      }

	  "with no session returns 403 (forbidden)" in {
	    for(session_res <- post("user/messages","message"->"test message")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/followees GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("user/followees")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/followees")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/followees POST" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.post("user/followees", "userId" -> "1")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(res <- post("user/followees", "userId" -> "1")) {
	      res.code must be equalTo 403
	    }
	  }
    } 

    "/user/followers GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("user/followers")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/followers")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/tracks GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("user/tracks")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- get("user/tracks")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

    "/user/tracks POST" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.post("user/tracks","track" -> ".*")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(session_res <- post("user/tracks","track" -> ".*")) {
	      session_res.code must be equalTo 403
	    }
	  }
    }

/*
 *   "/user/tracks/TRACKID DELETE" in {
 *     "with valid session" in {
 *       for {
 *         sess <- post_session
 *         create_track <- sess.post("user/tracks","track"->".*")
 *         res <- sess.delete("user/tracks/1")
 *       } {
 *         res.code must be equalTo 200
 *       }          
 *     }
 *
 *     "with no session returns 403 (forbidden)" in {
 *       for(session_res <- delete("user/tracks/1")) {
 *         session_res.code must be equalTo 403
 *       }
 *     }
 *   } 
 */

    "/user/actions GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("user/actions")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(res <- get("user/actions")) {
	      res.code must be equalTo 403
	    }
	  }
    }

    "/user/actions POST" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.post("user/actions",
                           "name"->"Test action",
                           "test"->"every 5 mins",
                           "action"->"rss:http://blog.com/feed.rss")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(res <- post("user/actions",
	                    "name"->"Test action",
	                    "test"->"every 5 mins",
	                    "action"->"rss:http://blog.com/feed.rss")) {
	      res.code must be equalTo 403
	    }
	  }
    }   

/*
 *    "/user/actions/ACTIONID PUT" in {
 *     "with valid session" in {
 *       for {
 *         sess <- post_session
 *         res <- sess.put("user/actions/1","enabled"->0)
 *       } {
 *         res.code must be equalTo 200
 *       }          
 *     }
 *
 *     "with no session returns 403 (forbidden)" in {
 *       for(res <- post("user/actions/1","enabled"->0)) {
 *         res.code must be equalTo 403
 *       }
 *     }
 *   }
 */

/*
 *   "/user/actions/ACTIONID DELETE" in {
 *     "with valid session" in {
 *       for {
 *         sess <- post_session
 *         res <- sess.delete("user/actions/1")
 *       } {
 *         res.code must be equalTo 200
 *       }          
 *     }
 *
 *     "with no session returns 403 (forbidden)" in {
 *       for(res <- delete("user/actions/1")) {
 *         res.code must be equalTo 403
 *       }
 *     }
 *   } 
 */

    "conversations/CONVERSATIONID GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("conversations/1")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(res <- get("conversations/1")) {
	      res.code must be equalTo 403
	    }
	  }
    }

    "/pools GET" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.get("pools")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(res <- get("pools")) {
	      res.code must be equalTo 403
	    }
	  }
    }

    "/pools POST" in {
      "with valid session" in {
        for {
          sess <- post_session
          res <- sess.post("pools","poolName"->"test_pool")
        } {
          res.code must be equalTo 200
        }          
      }

      "with no session returns 403 (forbidden)" in {
	    for(res <- post("pools","poolName"->"test_pool")) {
	      res.code must be equalTo 403
	    }
	  }
    }  

    "/pools/POOLID/users POST" in {

/*
 *     "with valid session" in {
 *       for {
 *         sess <- post_session
 *         res <- sess.post("pools/1/users",{"realm"->"test_realm";
 *                                                "userId"->1;
 *                                                "permission"->"Write"})
 *       } {
 *         res.code must be equalTo 200
 *       }          
 *     }
 */

      "with no session returns 403 (forbidden)" in {
	    for(res <- post("pools/1/users",{"realm"->"test_realm";
                                                 "userId"->2;
                                                 "permission"->"Write"})) {
	      res.code must be equalTo 403
	    }
	  }
    }        
  }  
}