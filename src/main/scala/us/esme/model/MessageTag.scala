package us.esme.model

/*
 * Copyright 2008 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */


import net.liftweb._
import mapper._
import util._
import Helpers._

object MessageTag extends MessageTag with LongKeyedMetaMapper[MessageTag] {

}

class MessageTag extends LongKeyedMapper[MessageTag] {
  def getSingleton = MessageTag // what's the "meta" server
  def primaryKeyField = id

  object id extends MappedLongIndex(this)
  
  object message extends MappedLongForeignKey(this, Message)
  object tag extends MappedLongForeignKey(this, Tag) // MappedText(this)
  object sentTo extends MappedLongForeignKey(this, User)
  object url extends MappedLongForeignKey(this, UrlStore)
}
