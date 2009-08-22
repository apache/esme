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
import TimeHelpers.intToTimeSpanBuilder
import TimeHelpers.timeSpanToLong
import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import org.apache.esme._
import model._
import org.apache.esme.actor._
import lib._
import view._
import snippet._
import api._
import net.liftweb._
import mapper._
import provider.HTTPRequest
import org.compass.core._
import org.compass.core.config.CompassConfiguration
import scala.actors.Actor
import Actor._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // do this before any messages are sent or there's hell to pay
    // ActorSchedulerFixer.doActorSchedulerFix()

    DefaultConnectionIdentifier.jndiName = Props.get("jndi.name") openOr "esme"

    if (!DB.jndiJdbcConnAvailable_?) DB.defineConnectionManager(DefaultConnectionIdentifier, DBVendor)
    // where to search snippet
    LiftRules.addToPackages("org.apache.esme")

    if (Props.mode == Props.RunModes.Test) {
      Schemifier.destroyTables_!!(Log.infoF _, User, ExtSession,
                                  Message, Mailbox, Tag,
                                  Group, Relationship, MessageTag,
                                  AuthToken, UrlStore, Tracking,
                                  Action, DidPerform, AccessPool,
                                  Privilege, UserAuth, UserCryptoSig)
    }

    Schemifier.schemify(true, Log.infoF _, User, ExtSession, Message,
                        Mailbox, Tag,
                        Group, Relationship, MessageTag, AuthToken,
                        UrlStore, Tracking, Action, DidPerform,
                        AccessPool, Privilege, UserAuth, UserCryptoSig)

    LiftRules.statelessDispatchTable.append {
      case r @ Req("api" :: "send_msg" :: Nil, "", PostRequest)
        if r.param("token").isDefined  =>
        () => RestAPI.sendMsgWithToken(r)
    }

    LiftRules.dispatch.append(ESMEOpenIDVendor.dispatchPF)

    LiftRules.siteMapFailRedirectLocation = List("static", "about")

    LiftRules.rewrite.prepend {
      case RewriteRequest(ParsePath("user" :: user :: Nil,"", _,_), _, _) =>
        RewriteResponse( List("info_view", "user"), Map("uid" -> user))
      case RewriteRequest(ParsePath("tag" :: tag :: Nil,"", _,_), _, _) =>
        RewriteResponse( List("info_view", "tag"), Map("tag" -> tag))

      case RewriteRequest(ParsePath("conversation" :: cid :: Nil, "", _, _),
                          _, _) =>
        RewriteResponse(List("user_view", "conversation"), Map("cid" -> cid))

      case RewriteRequest(ParsePath("search" :: term :: Nil,"", _,_), _, _) =>
        RewriteResponse( List("user_view", "search"), Map("term" -> term))
    }

    LiftRules.dispatch.append(UrlStore.redirectizer)


    LiftRules.siteMapFailRedirectLocation = List("static", "about")

    UserAuth.register(UserPwdAuthModule)
    UserAuth.register(OpenIDAuthModule)

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) ::
    Menu(Loc("list_users", List("user_view", "all"), "List Users")) ::
    Menu(Loc("user", List("info_view", "user"), "User Info", Hidden,
      Loc.Snippet("user_info", TagDisplay.userInfo))) ::
    Menu(Loc("conv", List("user_view", "conversation"), "Conversation", Hidden)) ::
    Menu(Loc("about", List("static", "about"), "About", Hidden)) ::
    Menu(Loc("tag", List("info_view", "tag"), "Tag", Hidden, Loc.Snippet("tag_display", TagDisplay.display))) ::
    Menu(Loc("search", List("user_view", "search"), "Search", Hidden)) ::
    Menu(Loc("sign_up", List("signup"), "Sign Up", 
             Snippet("signup", User.signupForm),
             Unless(User.loggedIn_? _, "Can't sign up when logged in"))) ::
    // User.sitemap :::
    TrackMgr.menuItems :::
    ActionMgr.menuItems :::
    AuthMgr.menuItems :::
    AccessPoolMgr.menuItems :::
    StreamMgr.menuItems

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
    ScalaInterpreter.touch
    
    PopStatsActor ! PopStatsActor.StartStats(ResendStat, 1 week, 1 hour)
    PopStatsActor ! PopStatsActor.StartStats(LinkClickedStat, 1 week, 1 hour)

    Action.findAll(By(Action.disabled, false), By(Action.removed, false)).foreach {
      _.startActors
    }

    DB.addLogFunc(S.logQuery _)
    S.addAnalyzer(RequestAnalyzer.analyze _)

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
    Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
    Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

        // Dump information about session every 10 minutes
    SessionMaster.sessionWatchers = SessionInfoDumper :: SessionMaster.sessionWatchers
  }
  private def makeUtf8(req: HTTPRequest) = {req.setCharacterEncoding("UTF-8")}
}

object Compass {
  // Set up Compass for search
  val conf = tryo(new CompassConfiguration()
                  .configure(Props.get("compass_config_file") openOr "/props/compass.cfg.xml")
                  .addClass((new Message).clazz))

  val compass = conf.map(_.buildCompass())

  for (c <- compass if !c.getSearchEngineIndexManager.indexExists)
  tryo(c.getSearchEngineIndexManager().createIndex())
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

object SessionInfoDumper extends Actor {
  private var lastTime = millis

  val tenMinutes: Long = 10 minutes
  def act = {
    link(ActorWatcher)
    loop {
      react {
        case SessionWatcherInfo(sessions) =>
          if ((millis - tenMinutes) > lastTime) {
            lastTime = millis
            val rt = Runtime.getRuntime
            rt.gc

            val dateStr: String = timeNow.toString
            Log.info("[MEMDEBUG] At "+dateStr+" Number of open sessions: "+sessions.size)
            Log.info("[MEMDEBUG] Free Memory: "+pretty(rt.freeMemory))
            Log.info("[MEMDEBUG] Total Memory: "+pretty(rt.totalMemory))
          }
      }
    }
  }

  private def pretty(in: Long): String =
  if (in > 1000L) pretty(in / 1000L)+","+(in % 1000L)
  else in.toString

  this.start
}
