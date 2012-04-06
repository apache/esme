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

 import org.eclipse.jetty.server.Server
 import org.eclipse.jetty.servlet.{DefaultServlet, ServletContextHandler}
 import org.eclipse.jetty.server.nio.SelectChannelConnector
 import net.liftweb.http.LiftFilter

 object JettyLauncher extends App {
   val port = if(System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080
   val server = new Server
   val scc = new SelectChannelConnector
   scc.setPort(port)
   server.setConnectors(Array(scc))

   val context = new ServletContextHandler(server, "/", ServletContextHandler.SESSIONS)
   context.addServlet(classOf[DefaultServlet], "/");
   context.addFilter(classOf[LiftFilter], "/*", 0)
   context.setResourceBase("src/main/webapp")

   server.start
   server.join
 }


