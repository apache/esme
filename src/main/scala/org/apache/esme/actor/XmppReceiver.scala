package org.apache.esme.actor

import akka.camel.{CamelMessage, Consumer}

import net.liftweb.common.{Empty, Logger}
import collection.immutable.Queue
import org.apache.esme.actor.Distributor.UserCreatedMessage
import org.apache.esme.model.User

object XmppReceiver {
  val logger: Logger = Logger("org.apache.esme.actor")
  case class FetchMessages()
}

class XmppReceiver(esmeSrv: String, esmePort: Int, esmeUsr: String, esmePwd: String, xmppServiceName: String, participant: String, user: User) extends Consumer {

  import XmppReceiver._

  var messages: List[(String, Long)] = List.empty

  def endpointUri = {val uri = "xmpp://%s@%s:%s/%s?password=%s" format (esmeUsr, esmeSrv, esmePort, participant, esmePwd); logger.info("XMPP URI is: %s".format(uri)); uri}

  def receive = {
    case msg: CamelMessage => {
      messages = (msg.bodyAs[String], System.currentTimeMillis) :: messages
    }
    case FetchMessages => {
      messages.foreach(message =>
        Distributor ! UserCreatedMessage(
          if (user != null) {user.id} else 0,
          message._1,
          Nil,
          message._2,
          Empty,
          participant,
          Empty,
          None
        )
      )
      messages = List.empty
    }
    case _ => logger.error("Incoming message is not Camel Message!")
  }
}