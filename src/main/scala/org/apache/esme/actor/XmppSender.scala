package org.apache.esme.actor

/**
 * User: vvivanov
 * Date: 7/2/12
 * Time: 8:16 PM
 */

import org.apache.esme.model._

import akka.actor.Actor
import akka.camel.{CamelMessage, Producer, Oneway}


import scalaz.{Scalaz}
import Scalaz._

import net.liftweb.common.Logger


object XmppSender {
  case class XMPPMsg(action: Performances, msg: Message, user: User, reason: MailboxReason, token: String)
  val logger: Logger = Logger("org.apache.esme.actor")
}

class XmppSender(esmeSrv: String, esmePort: Int, esmeUsr: String, esmePwd: String, xmppServiceName: String) extends Actor with Producer with Oneway  {
  import XmppSender._

  // Ugly, but at the moment there's no other way
  // to set participant dynamically - populating corresponding
  // header for outgoing message doesn't work for some reason
  var participant: String = _

  def endpointUri = {val uri = "xmpp://%s@%s:%s/%s?password=%s" format (esmeUsr, esmeSrv, esmePort, participant, esmePwd); logger.info("XMPP URI is: %s".format(uri)); uri}

  override protected def transformOutgoingMessage(msg: Any) = msg match {
    case XMPPMsg(action, msg, user, reason, token) => {
      (action: @unchecked) match {
        case XmppTo(who, body) =>
          participant = who
          new CamelMessage(body.cata(s => s, ""), Map("participant" -> ("%s" format who), "serviceName" -> xmppServiceName))
      }
    }
    case _ =>
  }

  /*
  override protected def transformResponse(msg : Any) = msg match {
    case resp: akka.actor.Status.Failure =>
      akka.actor.Status.Failure(resp.cause)
    case _ => msg
  }
  */
}
