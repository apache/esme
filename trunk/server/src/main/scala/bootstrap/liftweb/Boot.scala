package bootstrap.liftweb

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

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.auth._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import org.apache.esme._
import model._
import actor._
import lib._
import view._
import api._
import net.liftweb._
import mapper._
import javax.servlet.http.{HttpServlet, HttpServletRequest , HttpServletResponse, HttpSession}
import org.compass.core._
import org.compass.core.config.CompassConfiguration

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    DefaultConnectionIdentifier.jndiName = Props.get("jndi.name") openOr "esme"
    
    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    // where to search snippet
    LiftRules.addToPackages("org.apache.esme")
    
    if (Props.mode == Props.RunModes.Test) {
      Schemifier.destroyTables_!!(Log.infoF _, User, ExtSession,
                                  Message, Mailbox, Tag,
                                  Group, Relationship, MessageTag, 
                                  AuthToken, UrlStore, Tracking,
                                  Action, DidPerform)
    }
    
    Schemifier.schemify(true, Log.infoF _, User, ExtSession, Message,
                        Mailbox, Tag,
                        Group, Relationship, MessageTag, AuthToken, 
                        UrlStore, Tracking, Action, DidPerform)
    
    LiftRules.statelessDispatchTable.append {
      case r @ Req("api" :: "send_msg" :: Nil, "", PostRequest) 
        if r.param("token").isDefined  => 
        () => RestAPI.sendMsgWithToken(r)
    }
    
    LiftRules.dispatch.append(ESMEOpenIDVendor.dispatchPF)
    
    LiftRules.siteMapFailRedirectLocation = List("static", "about")
    
    LiftRules.rewrite.prepend {
      case RewriteRequest(ParsePath("user" :: user :: Nil,"", _,_), _, _) =>
        RewriteResponse( List("user_view", "index"), Map("uid" -> user))
      case RewriteRequest(ParsePath("tag" :: tag :: Nil,"", _,_), _, _) =>
        RewriteResponse( List("user_view", "tag"), Map("tag" -> tag))
        
      case RewriteRequest(ParsePath("conversation" :: cid :: Nil, "", _, _),
                          _, _) => 
        RewriteResponse(List("user_view", "conversation"), Map("cid" -> cid))

      case RewriteRequest(ParsePath("search" :: term :: Nil,"", _,_), _, _) =>
        RewriteResponse( List("user_view", "search"), Map("term" -> term))
    }

    LiftRules.dispatch.append(UrlStore.redirectizer)


    LiftRules.siteMapFailRedirectLocation = List("static", "about")

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) ::
    Menu(Loc("list_users", List("user_view", "all"), "List Users")) ::
    Menu(Loc("user", List("user_view", "index"), "User Info", Hidden)) ::
    Menu(Loc("conv", List("user_view", "conversation"), "Conversation", Hidden)) ::
    Menu(Loc("about", List("static", "about"), "About", Hidden)) ::
    Menu(Loc("tag", List("user_view", "tag"), "Tag", Hidden)) ::
    Menu(Loc("search", List("user_view", "search"), "Search", Hidden)) ::
    User.sitemap :::
    Track.menuItems :::
    Auth.menuItems :::
    ActionView.menuItems

    LiftRules.setSiteMap(SiteMap(entries:_*))
    
    S.addAround(ExtSession.requestLoans)

    LiftRules.viewDispatch.append {
      case "user_view" :: _ => Right(UserView)
    }

    LiftRules.dispatch.prepend(RestAPI.dispatch)

    LiftRules.httpAuthProtectedResource.prepend {
     case (ParsePath(TwitterAPI.ApiPath :: _, _, _, _)) => Full(AuthRole("user"))
    }
    
    LiftRules.authentication = TwitterAPI.twitterAuth

    LiftRules.dispatch.append(TwitterXmlAPI.dispatch)
    LiftRules.dispatch.append(TwitterJsonAPI.dispatch)
    
    LiftRules.early.append(makeUtf8)

    Distributor.touch
    SchedulerActor.touch
    MessagePullActor.touch

    Action.findAll(By(Action.disabled, false), By(Action.removed, false)).foreach {
      _.startActors
    }

    DB.addLogFunc(S.logQuery _)
    S.addAnalyzer(RequestAnalyzer.analyze _)
  }
  private def makeUtf8(req: HttpServletRequest): Unit = {req.setCharacterEncoding("UTF-8")}
}

object Compass {
  // Set up Compass for search
  val conf = new CompassConfiguration()
    .configure(Props.get("compass_config_file") openOr "/props/compass.cfg.xml")
    .addClass((new Message).clazz)
    
  val compass = conf.buildCompass()
  if (!compass.getSearchEngineIndexManager.indexExists)
  {
    // Create Index if one does not exist
    compass.getSearchEngineIndexManager().createIndex()
  }
}


object RequestAnalyzer {

  def analyze(req: Box[Req], time: Long, queries: List[(String, Long)]): Unit = {
    val longQueries = (queries.filter(_._2 > 30))
    if (time > 50 || longQueries.?) {
      Log.info("Request "+req.map(_.uri).openOr("No Request")+
               " took "+time+" query "+longQueries.comma)
    }
  }
}

object DBVendor extends ConnectionManager {
  private var pool: List[Connection] = Nil
  private var poolSize = 0
  private val maxPoolSize = 4
  
  private def createOne: Box[Connection] = try {
    if (Props.getBool("use_prod_psql", false)) {
      Class.forName("org.postgresql.Driver")
      val dm = DriverManager.
      getConnection("jdbc:postgresql://localhost/esme_prod",
                    Props.get("db_user", "dpp"),
                    Props.get("db_pwd", ""))
      Full(dm)
    } else if (Props.getBool("use_local_psql", false)) {
      Class.forName("org.postgresql.Driver")
      val dm = DriverManager.
      getConnection("jdbc:postgresql://localhost/esme_dev",
                    Props.get("db_user", "dpp"),
                    Props.get("db_pwd", ""))
      Full(dm)
    } else {
      Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
      val driverName = Props.mode match {
        case Props.RunModes.Test => "jdbc:derby:esme_test_db;create=true"
        case _ => "jdbc:derby:esme_db;create=true"
      }
      
      val dm = DriverManager.getConnection(driverName)
      Full(dm)
    }
  } catch {
    case e : Exception => e.printStackTrace; Empty
  }
  
  def newConnection(name: ConnectionIdentifier): Box[Connection] = synchronized {
    pool match {
      case Nil if poolSize < maxPoolSize => val ret = createOne
        poolSize = poolSize + 1
        ret.foreach(c => pool = c :: pool)
        ret

      case Nil => wait(1000L); newConnection(name)
      case x :: xs => try {
          x.setAutoCommit(false)
          Full(x)
        } catch {
          case e => try {
              pool = xs
              poolSize = poolSize - 1
              x.close
              newConnection(name)
            } catch {
              case e => newConnection(name)
            }
        }
    }
  }
  
  def releaseConnection(conn: Connection): Unit = synchronized {
    pool = conn :: pool
    notify
  }
}

