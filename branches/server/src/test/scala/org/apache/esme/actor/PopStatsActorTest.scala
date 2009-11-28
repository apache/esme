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

package org.apache.esme.actor

import org.specs._
import org.specs.runner._
import org.specs.matcher._
import org.apache.esme.actor._

class PopStatsActorSpecsAsTest extends JUnit3(PopStatsActorSpecs)
object PopStatsActorSpecsRunner extends ConsoleRunner(PopStatsActorSpecs)

object PopStatsActorSpecs extends Specification {
  
  "Popularity Stats" should {
    "Update top hits after inrcementing statistics" in {
      PopStatsActor !? (100, PopStatsActor.StartStats(ResendStat, 2000, 1000))

      PopStatsActor !? (100, PopStatsActor.IncrStats(ResendStat, 1))
      val stats1 = PopStatsActor !? PopStatsActor.TopStats(ResendStat, 5, 2000)
      stats1 must beEqualTo(List(1 -> 1))

      PopStatsActor !? (100, PopStatsActor.IncrStats(ResendStat, 3))
      val stats2 = PopStatsActor !? PopStatsActor.TopStats(ResendStat, 5, 2000)
      stats2 must beEqualTo(List(3 -> 1, 1 -> 1))

      PopStatsActor !? (100, PopStatsActor.IncrStats(ResendStat, 1))
      val stats3 = PopStatsActor !? PopStatsActor.TopStats(ResendStat, 5, 2000)
      stats3 must beEqualTo(List(1 -> 2, 3 -> 1))
    }
    
    "Information should expire after timeout" in {
      Thread.sleep(2000)
      val stats = PopStatsActor !? PopStatsActor.TopStats(ResendStat, 5, 2000)
      stats must beEqualTo(Nil)
    }
  }
}
