/*
 Licensed to the Apache Software Foundation (ASF) under one   *
 or more contributor license agreements.  See the NOTICE file *
 distributed with this work for additional information        *
 regarding copyright ownership.  The ASF licenses this file   *
 to you under the Apache License, Version 2.0 (the            *
 "License"); you may not use this file except in compliance   *
 with the License.  You may obtain a copy of the License at   *
                                                              *
   http://www.apache.org/licenses/LICENSE-2.0                 *
                                                              *
 Unless required by applicable law or agreed to in writing,   *
 software distributed under the License is distributed on an  *
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY       *
 KIND, either express or implied.  See the License for the    *
 specific language governing permissions and limitations      *
 under the License.                                           *
*/

package org.apache.esme.api;

import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.esme.api.EsmeRestApi;
import junit.framework.TestCase;
import org.apache.esme.model.Message;
import org.apache.esme.model.Status;

public class RestTest extends TestCase {

    private static Logger logger = Logger.getLogger("org.apache.esme.api");
    private static String token = TestProperties.getProperty("esme-token");
    private static String esmeServer = TestProperties.getProperty("esme-server");
    private String testMsg = "This is a background message to be picked up by testWaitForMessages";
    private EsmeRestApi esme;

    public void testStatus() throws Exception
    {
        EsmeRestApi.debugMode = true;

        logger.log(Level.INFO, "Testing message status");

        esme = new EsmeRestApi(esmeServer);
        Status status = esme.getStatus();

        logger.log(Level.INFO, "API status is "+status);

        assertNull(status); // status is null because we have not logged in
}

	public void testSendMsg() throws Exception
    {
//		EsmeRestApi esme = new EsmeRestApi("http://api.esme.us/esme/api");
//		esme.setProxy("proxy",8080);
//		esme.login("JHUWLD5YQ1EY3UWTSOIQMNCHNQO4EYED");
//		Message msg = new Message();
//		msg.setText("Hello World, from Java, via SAP proxy server");
//		msg.setTags(new String[]{"Java Api","proxy"});
//      esme.sendMsg(msg);
//      esme.logout();

        EsmeRestApi.debugMode = true;

        logger.log(Level.INFO, "Testing message sending");

        esme = new EsmeRestApi(esmeServer);

        logger.log(Level.INFO, "Login with token "+token);
        esme.login(token);

        Status status = esme.getStatus();
        assertNotNull(status);
        logger.log(Level.INFO, "API status is "+status);

        esme.sendMsg("Hello World, from Java - the status was "+status);
        
        logger.log(Level.INFO, "Logout");
        esme.logout();
    }

	public void testGetMessages() throws Exception
    {
        EsmeRestApi.debugMode = true;

        logger.log(Level.INFO, "Testing getMessages");

        esme = new EsmeRestApi(esmeServer);
        esme.login(token);

        Status status = esme.getStatus();
        assertNotNull(status);
        logger.log(Level.INFO, "API status is "+status);

        List messages = esme.getMessages();
        assertNotNull(messages);

        logger.info("Got "+messages.size()+" messages from server.");

        esme.logout();
    }

	public void testWaitForMessages() throws Exception
    {
        EsmeRestApi.debugMode = true;

        logger.log(Level.INFO, "Testing waitForMessages");

        esme = new EsmeRestApi(esmeServer);
        esme.login(token);

        Status status = esme.getStatus();
        assertNotNull(status);
        logger.log(Level.INFO, "API status is "+status);

        // Get the initial message list
        List messages = esme.getMessages();

        // Kick off a thread to wait 5 seconds and then send a message
        new BackgroundMessage().start();

        // Now we wait for the new message to appear
        messages = esme.waitForMessages();
        assertNotNull(messages);

        logger.info("Got "+messages.size()+" messages from server.");

        assertNotNull((Message)messages.get(0));
        assertEquals(testMsg, ((Message)messages.get(0)).getBody());
        
        esme.logout();
    }

    private class BackgroundMessage extends Thread
    {
        public void run()
        {
            try {
                // Log into ESME, wait 5 seconds, send a message & logout
                EsmeRestApi bgEsme = new EsmeRestApi(esmeServer);
                bgEsme.login(token);
                sleep(5000);
                bgEsme.sendMsg(testMsg);
                bgEsme.logout();
            } catch (IOException ex) {
                ex.printStackTrace();
            } catch (InterruptedException ex) {
                ex.printStackTrace();
            }

        }
    }
}
