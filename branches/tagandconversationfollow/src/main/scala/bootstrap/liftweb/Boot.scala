package bootstrap.liftweb

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

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.http._
import net.liftweb.http.auth._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._
import TimeHelpers.intToTimeSpanBuilder
//import net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, ConnectionIdentifier}
import java.sql.{Connection, DriverManager}
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import org.apache.esme._
import model._
import org.apache.esme.actor._
import lib._
import snippet._
import api._
import net.liftweb._
import common.Full
import mapper._
import provider.HTTPRequest
import org.compass.core._
import org.compass.core.config.CompassConfiguration

import net.liftweb.widgets.tablesorter._
//import com.twitter.stats._
import com.twitter.ostrich.{ServiceTracker, Stats, StatsMBean}
import net.lag.configgy.{RuntimeEnvironment, Config}

import _root_.net.liftweb.widgets.logchanger._



/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {

    // Get DB from container if available
    
    DefaultConnectionIdentifier.jndiName = Props.get("jndi.name") openOr "esme"

    // Deal with Database
        
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
	new StandardDBVendor(Props.get("db_driver") openOr "org.apache.derby.jdbc.EmbeddedDriver",
			     Props.get("jdbc_connect_url") openOr 
			     "jdbc:derby:esme_db;create=true",
			     Props.get("db_user"), Props.get("db_pwd"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }
    
    // where to search snippet
    LiftRules.addToPackages("org.apache.esme")

    if (Props.mode == Props.RunModes.Test) {
      Schemifier.destroyTables_!!(Log.infoF _, User, ExtSession,
        Message, Mailbox, Tag, UserTagFollow, UserConvFollow,
        Relationship, MessageTag,
        AuthToken, UrlStore, Tracking,
        Action, AccessPool,
        Privilege, UserAuth, UserCryptoSig)
    }

    Schemifier.schemify(true, Log.infoF _, User, ExtSession, Message,
      Mailbox, Tag, UserTagFollow, UserConvFollow,
      Relationship, MessageTag, AuthToken,
      UrlStore, Tracking, Action, 
      AccessPool, Privilege, UserAuth, UserCryptoSig)


    // deal with sending token via URL for old API
    LiftRules.statelessDispatchTable.append {
      case r@Req("api" :: "send_msg" :: Nil, "", PostRequest)
        if r.param("token").isDefined =>
        () => RestAPI.sendMsgWithToken(r)
    }
    
    //Added exceptions to the log
    LiftRules.exceptionHandler.prepend {
      case (_, _, exception) => {
        Log.error(exception.getStackTrace.toString)
        RedirectResponse("/")
      }
    } 
    
    
  

    
    //Add logchanger
    import _root_.net.liftweb.widgets.logchanger._
    LogLevelChanger.init

    object logLevel extends LogLevelChanger with Log4jLoggingBackend


    LiftRules.dispatch.append(ESMEOpenIDVendor.dispatchPF)

    //Resources for Internationalization
    LiftRules.resourceNames = "ESMECustom" :: "ESMEBase" :: "ESMEUI" :: Nil

    //Jquery functions

    TableSorter.init

     //Dealing with URL-based request parameters
     
    LiftRules.rewrite.prepend {
      case RewriteRequest(ParsePath("user" :: user :: Nil, "", _, _), _, _) =>
        RewriteResponse(List("info_view", "user"), Map("uid" -> user))
      case RewriteRequest(ParsePath("tag" :: tag :: Nil, "", _, _), _, _) =>
        RewriteResponse(List("info_view", "tag"), Map("tag" -> tag))

      case RewriteRequest(ParsePath("conversation" :: cid :: Nil, "", _, _),
      _, _) =>
        RewriteResponse(List("info_view", "conversation"), Map("cid" -> cid))

      case RewriteRequest(ParsePath("search" :: term :: Nil, "", _, _), _, _) =>
        RewriteResponse(List("info_view", "search"), Map("term" -> term))
    }

    LiftRules.dispatch.append(UrlStore.redirectizer)

    //What to do when user types in something unknown 
         
    LiftRules.siteMapFailRedirectLocation = List("index")


    // Register Auth methods that are used in ESME
    
    UserAuth.register(UserPwdAuthModule)
    UserAuth.register(OpenIDAuthModule)

    // Build SiteMap
    val entries = Menu(Loc("Home", List("index"), "Home")) ::
        Menu(Loc("user", List("info_view", "user"), "User Info", Hidden,
          Loc.Snippet("user_info", TagDisplay.userInfo))) ::
        logLevel.menu  ::
        Menu(Loc("tag", List("info_view", "tag"), "Tag", Hidden, Loc.Snippet("tag_display", TagDisplay.display))) ::
        Menu(Loc("public", List("info_view", "public"), S.?("base_profile_public"))) ::
        Menu(Loc("contacts", List("info_view", "contacts"), S.?("base_profile_contacts"))) ::
        Menu(Loc("sign_up", List("signup"), S.?("base_menu_signup"),
          Snippet("signup", User.signupForm),
          Unless(User.loggedIn_? _, S.?("base_menu_sign_up_error")))) ::
        Menu(Loc("logout", List("logout"), S.?("base_menu_logout"),
          EarlyResponse(() => {User.logUserOut; S.redirectTo(S.referer openOr "/")}),
          If(User.loggedIn_? _, S.?("base_menu_logout_error")))) ::
        // User.sitemap :::
        UserMgr.menuItems :::
        TrackMgr.menuItems :::
        ActionMgr.menuItems :::
        AuthMgr.menuItems :::
        AccessPoolMgr.menuItems :::
        StreamMgr.menuItems :::
        ConversationMgr.menuItems :::
        SearchMgr.menuItems :::
        ProfileMgr.menuItems

    LiftRules.setSiteMap(SiteMap(entries: _*))

    S.addAround(ExtSession.requestLoans)          

    // API security rules
    LiftRules.dispatch.append(API2.authorizationRules)

    // REST APIs (new and old)
    LiftRules.dispatch.prepend(RestAPI.dispatch)
    LiftRules.dispatch.append(API2.dispatch)

      // Functionality assocaited with ESME support of twitter api 
    LiftRules.httpAuthProtectedResource.prepend {
      case Req(TwitterAPI.ApiPath :: _,_,_) => Full(AuthRole("user"))
    } 

    LiftRules.authentication = TwitterAPI.twitterAuth
                               
    LiftRules.dispatch.append(TwitterXmlAPI.dispatch)
    LiftRules.dispatch.append(TwitterJsonAPI.dispatch)

    LiftRules.early.append(makeUtf8)

    //JMX
    if (Props.getBool("jmx.enable", false))
      StatsMBean("org.apache.esme.stats")
      
    Stats.makeGauge("users") {Distributor.getUsersCount}
    Stats.makeGauge("listener") {Distributor.getListenersCount}

    val runtime = new RuntimeEnvironment(getClass)
    val config = new Config
    config("admin_text_port") = Props.getInt("admin_text_port") openOr 9989
    config("admin_http_port") = Props.getInt("admin_http_port") openOr 9990
    ServiceTracker.startAdmin(config, runtime)

    // start Scala Actors used in ESME
    Distributor.touch
    SchedulerActor.touch
    MessagePullActor.touch   
    TagDistributor.touch  
    ConvDistributor.touch
    // ScalaInterpreter.touch


    // Initiating popular links and resent messages
    val resentPeriod = Props.getLong("stats.resent.period", 1 week)
    val resentRefreshInterval: Long = Props.getLong("stats.resent.refresh") match {
      case Full(interval) if interval > (1 minute) => interval
      case _ => 1 hour
    }
    val linksPeriod = Props.getLong("stats.links.period", 1 week)
    val linksRefreshInterval: Long = Props.getLong("stats.links.refresh") match {
      case Full(interval) if interval > (1 minute) => interval
      case _ => 1 hour
    }
    if (resentPeriod > 0)
      PopStatsActor ! PopStatsActor.StartStats(ResendStat, resentPeriod, resentRefreshInterval)
    if (linksPeriod > 0)
      PopStatsActor ! PopStatsActor.StartStats(LinkClickedStat, linksPeriod, linksRefreshInterval)

    // Initiating actions
    Action.findAll(By(Action.disabled, false), By(Action.removed, false)).foreach {
      _.startActors
    }

    // DB.addLogFunc(S.logQuery _)
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

/*
* Set up compass search environment
*/

object Compass {
  val conf = tryo(new CompassConfiguration()
      .configure(Props.get("compass_config_file") openOr "/props/compass.cfg.xml")
      .addClass((new Message).clazz))

  val compass = conf.map(_.buildCompass())

  for (c <- compass if !c.getSearchEngineIndexManager.indexExists)
    tryo(c.getSearchEngineIndexManager().createIndex())
}

/*
* Logger to track request times
*/

object RequestAnalyzer {
  def analyze(req: Box[Req], time: Long, queries: List[(String, Long)]): Unit = {
    val longQueries = (queries.filter(_._2 > 30))
    if (time > 50 || longQueries.?) {
      Log.debug("Request " + req.map(_.uri).openOr("No Request") +
          " took " + time + " query " + longQueries.comma)
    }
  }
}

/*
* Logger to dump session info
*/
object SessionInfoDumper extends LiftActor {
  private var lastTime = millis

  val tenMinutes: Long = 10 minutes

  protected def messageHandler = {
    case SessionWatcherInfo(sessions) =>
      Stats.getCounter("liftSessions").update(sessions.size)
      if ((millis - tenMinutes) > lastTime) {
        lastTime = millis
        val rt = Runtime.getRuntime
        rt.gc

        val dateStr: String = timeNow.toString
        Log.debug("[MEMDEBUG] At " + dateStr + " Number of open sessions: " + sessions.size)
        Log.debug("[MEMDEBUG] Free Memory: " + pretty(rt.freeMemory))
        Log.debug("[MEMDEBUG] Total Memory: " + pretty(rt.totalMemory))
      }

  }

  private def pretty(in: Long): String =
    if (in > 1000L) pretty(in / 1000L) + "," + (in % 1000L)
    else in.toString
}
