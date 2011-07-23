package org.apache.esme.model

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

import net.liftweb._
import mapper._
import util._
import common._
import http._

import org.apache.esme.actor.PopStatsActor
import org.apache.esme.actor.LinkClickedStat

object UrlStore extends UrlStore with LongKeyedMetaMapper[UrlStore] {
  def redirectizer: LiftRules.DispatchPF = {
    case Req("u" :: id :: Nil, "", GetRequest) =>
      serve(id)
  }

  private def serve(id: String)(): Box[LiftResponse] = 
  for (url <- find(By(uniqueId, id)))
  yield {
    PopStatsActor ! PopStatsActor.IncrStats(LinkClickedStat, url.id)
    RedirectResponse(url.url)
  }

  def make(in: String): UrlStore = {
    find(By(url, in)) match {
      case Full(r) => r
      case _ => UrlStore.create.url(in).saveMe
    }
  }
  
  def urlFrom(in: String): String =
  find(By(uniqueId, in)).map(_.url.is) openOr
  "http://google.com"
}

class UrlStore extends LongKeyedMapper[UrlStore] {
  def getSingleton = UrlStore // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)

  object url extends MappedPoliteString(this, 512) {
    override def validations =
    this.valMinLen(3, S.?("base_urlstore_err_too_short")) _ ::
    super.validations

    override def setFilter = trim _ :: super.setFilter

    override def dbIndexed_? = true
  }

  object uniqueId extends MappedUniqueId(this, 16) {
    override def dbIndexed_? = true
  }
}
