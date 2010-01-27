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
import Helpers._

import java.security._

/**
 * The model object that contains the cryptographic information
 */
class UserCryptoSig extends LongKeyedMapper[UserCryptoSig] with IdPK {
  def getSingleton = UserCryptoSig

  object user extends MappedLongForeignKey(this, User)
  object createdOn extends MappedLong(this) {
    override def defaultValue = millis
  }
  object valid extends MappedBoolean(this) {
    override def defaultValue = true
  }
  object publicKey extends MappedString(this, 1024) {
    override def dbIndexed_? = true
    override def defaultValue = base64Encode(defaultKeyPair.getPublic.getEncoded)
  }

  object privateKey extends MappedText(this) {
    override def defaultValue = base64Encode(defaultKeyPair.getPrivate.getEncoded)
  }

  private lazy val defaultKeyPair: KeyPair = 
  KeyPairGenerator.getInstance("rsa").generateKeyPair

  // lazy val publicKeyAsX509
  

}

object UserCryptoSig extends UserCryptoSig with LongKeyedMetaMapper[UserCryptoSig]
