package org.apache.esme.actor

import akka.actor.{ActorRef, Actor, Props => AkkaProps}
import net.liftweb.util.Props
import org.apache.esme.actor.XmppReceiver.FetchMessages
import org.apache.esme.model.User
import net.liftweb.common.Logger

object XmppSupervisor {
  val logger: Logger = Logger("org.apache.esme.actor")


  sealed trait XmppSupervisorActions
  case class Fetch(id: Long) extends XmppSupervisorActions
  case class Start(id: Long, who: String, usr: User) extends XmppSupervisorActions
  case class Stop(id: Long) extends XmppSupervisorActions
}

class XmppSupervisor extends Actor {

  import XmppSupervisor._

  private var xmppPullActors: Map[Long, ActorRef] = Map.empty

  var xmppHost: String = _
  var xmppPort: String = _
  var xmppUsr: String = _
  var xmppPwd: String = _
  var xmppServiceName: String = _


  override def preStart() {
    logger.info("preStart() called")

    xmppHost = Props.get("xmpp.host") openOr ""
    xmppPort = Props.get("xmpp.port") openOr ""
    xmppUsr = Props.get("xmpp.user") openOr ""
    xmppPwd = Props.get("xmpp.password") openOr ""
    xmppServiceName = Props.get("xmpp.serviceName") openOr ""
  }

  def receive = {
    case Start(id, who, usr) => {
      logger.info("Start message received. User: %s, who: %s".format(usr, who))
      xmppPullActors += (id -> context.actorOf(AkkaProps(new XmppReceiver(xmppHost, xmppPort.toInt, xmppUsr, xmppPwd, xmppServiceName, who, usr))))
    }
    case Stop(id) => {
      xmppPullActors.get(id).foreach { ref =>
        context.stop(ref)
        xmppPullActors -= id
      }
    }
    case Fetch(id) => {
      logger.info("Fetch message received")
      xmppPullActors.get(id).foreach(ref => ref ! FetchMessages)
    }
    case _ => logger.info("Unknown message received")
  }
}
