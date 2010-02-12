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

import java.io.ByteArrayInputStream;
import org.apache.esme.model.Message;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.commons.httpclient.*;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.PostMethod;
import org.apache.esme.model.Status;
import org.w3c.dom.*;
import org.xml.sax.SAXException;

/**
 * @author I056593
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class EsmeRestApi {

    public static boolean debugMode = false;
    private static Logger logger = Logger.getLogger("org.apache.esme.api");

	private String apiUrl;
	private String authToken;
	private HttpClient client = new HttpClient();
	//private HostConfiguration hostConfig = new HostConfiguration();

	public void setProxy(String proxyHost, int proxyPort) {
		//hostConfig.setProxy(proxyHost, proxyPort);
		client.getHostConfiguration().setProxy(proxyHost, proxyPort);
	}

	public EsmeRestApi(String apiUrl) throws IOException {
		this.apiUrl = apiUrl;
	}

	public void sendMsg(String message) {
		Message msg = new Message();
		msg.setBody(message);
		sendMsg(msg);
	}

	/**
	 * @return
	 */
	public String getApiUrl() {
		return apiUrl;
	}

	/**
	 * @param string
	 */
	public void setApiUrl(String string) {
		apiUrl = string;
	}

	/**
	 * @return
	 */
	public String getAuthToken() {
		return authToken;
	}

	/**
	 * @param string
	 */
	public void setAuthToken(String string) {
		authToken = string;
	}

    private Document executeHttp(HttpMethod method)
    {
        Document result = null;

        try {
            int statusCode = client.executeMethod(method);
            if (statusCode != 200) {
                logger.log(Level.SEVERE, "send_msg response code: " + statusCode);
                throw new EsmeException(statusCode);
            }
            byte[] responseBody = method.getResponseBody();
            logger.log(Level.INFO, "Got body: "+new String(responseBody,"utf-8"));
            
            // Parse the resulting XML into a DOM
            ByteArrayInputStream statusStream = new ByteArrayInputStream(responseBody);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db;
            db = dbf.newDocumentBuilder();
            logger.log(Level.INFO, "Parse & create Document");
            result = db.parse(statusStream);
            logger.log(Level.INFO,"Parsed.");
		} catch (IOException e) {
			logger.log(Level.SEVERE,"Fatal transport error or XML error",e);
        } catch (SAXException e) {
			logger.log(Level.SEVERE,"Fatal XML error",e);
        } catch (ParserConfigurationException e) {
			logger.log(Level.SEVERE,"Fatal XML error",e);
        } finally {
            return result;
        }
    }

    public void login(String token)
	{
//		POST /api/login
//		token=API_TOKEN
//
//		<esme_api operation="login" success="true"/>

		PostMethod method = new PostMethod(apiUrl + "/login");
		NameValuePair[] data = { new NameValuePair("token", token) };
        method.setRequestBody(data);
		try {
            Document document = executeHttp(method);
		} finally {
			// Release the connection.
			method.releaseConnection();
		}
	}

	public void logout()
	{
//		GET /api/logout
//
//		<esme_api operation="logout" success="true"/>

		GetMethod method = new GetMethod(apiUrl + "/logout");
		try {
            Document document = executeHttp(method);
		} finally {
			// Release the connection.
			method.releaseConnection();
		}
	}

	public void sendMsg(Message message) {
//			POST /api/send_msg
//			message=messagebody
//			via=optional_name_of_client
//			tags=optional_comma_delimitted_tags
//			metadata=optional_XML_Element_data
//			replyto=optional_id_of_message

// 			<esme_api operation="send_msg" success="true"/>

		PostMethod method = new PostMethod(apiUrl + "/send_msg");
		String tags="";
		for (int i = 0; i < message.getTags().length; i++) {
			if (i != 0)	tags+=(",");
			tags+=(message.getTags()[i]);
		}
		NameValuePair[] data = {
			new NameValuePair("message", message.getBody()),
			new NameValuePair("via", message.getSource()),
			new NameValuePair("tags", tags),
		};
		try {
            method.setRequestBody(data);
            Document document = executeHttp(method);
		} finally {
			// Release the connection.
			method.releaseConnection();
		}
	}

    public Status getStatus()
    {
//      GET /api/status
//
//      <esme_api operation="status" success="false"/>  -> null

//      <esme_api operation="status" success="true">  -> Status object
//        <user nickname="..." id="1"/>
//      </esme_api>

        GetMethod method = new GetMethod(apiUrl + "/status");
        Status status = new Status();

		try {
            Document document = executeHttp(method);

            logger.log(Level.INFO, "Get Success string");
            String success = document.getDocumentElement().getAttribute("success");

            logger.log(Level.INFO, "success = "+success);
            if ("false".equals(success))
            {
                logger.severe("success=false when calling /status");
                return null;
            }
            Element user = (Element) document.getElementsByTagName("user").item(0);

        /*
         * <user nickname="dhague"
         *       image="http://www.gravatar.com/avatar/c438262bd9c0caedbe3312c1ded261f6?s=80"
         *       id="1"
         *       whole_name="Darren Hague"/>
         */
            status.setId(user.getAttribute("id"));
            status.setImage(user.getAttribute("image"));
            status.setNickname(user.getAttribute("nickname"));
            status.setWholeName(user.getAttribute("whole_name"));

		} finally {
			// Release the connection.
			method.releaseConnection();
		}

        return status;
    }

    public List getMessages()
    {
        return getMessagesHelper("/get_msgs");
    }

    public List waitForMessages()
    {
        return getMessagesHelper("/wait_for_msgs");
    }

    private List getMessagesHelper(String apiCall)
    {
//  GET /api/get_msgs
//
//  <esme_api operation="get_msgs" success="true">
//        <message id="1134" source="web" date="...">
//                <author image="..." name="..." id="9"/>
//                <body>
//                        Woohoo - got through the rest of the slides quite quickly. I can go home now.
//                </body>
//                <tags>
//                       <tag id="12" name="Tags"/>
//                       <tag id="11" name="Compass"/>
//                </tags>
//        </message>
//  </esme_api>

        // TODO
        GetMethod method = new GetMethod(apiUrl + apiCall);

        List messageList = new ArrayList();

		try {
            Document document = executeHttp(method);

            logger.info("Check success");

            String success = document.getDocumentElement().getAttribute("success");
            if ("false".equals(success))
            {
                logger.severe("success=false when calling /get_msgs");
                return null;
            }

            NodeList messages =  document.getElementsByTagName("message");

            for (int i=0; i<messages.getLength(); i++)
            {
                Message msg = new Message();
                Element nodeMessage = (Element)messages.item(i);

                msg.setMessageId(nodeMessage.getAttribute("id"));
                msg.setSource(nodeMessage.getAttribute("source"));
                msg.setDate(nodeMessage.getAttribute("date"));

                logger.info("Got message basics, now get author info");

                Element nodeAuthor = (Element)nodeMessage.getElementsByTagName("author").item(0);
                if (nodeAuthor != null)
                {
                    msg.setAuthorId(nodeAuthor.getAttribute("id"));
                    msg.setAuthorName(nodeAuthor.getAttribute("name"));
                    msg.setAuthorImage(nodeAuthor.getAttribute("image"));
                }

                logger.info("Got author, now get body");

                Element nodeBody = (Element)nodeMessage.getElementsByTagName("body").item(0);
                if (nodeBody != null)
                {
                    msg.setBody(nodeBody.getTextContent());
                }

                logger.info("Got body, now get tags");

                NodeList nodeTags = (NodeList)nodeMessage.getElementsByTagName("tags");
                ArrayList tagList = new ArrayList();
                for (int j=0; j<nodeTags.getLength(); j++)
                {
                    Element nodeTag = (Element)nodeTags.item(j);
                    tagList.add((String)nodeTag.getAttribute("name"));
                }
                msg.setTags((String[])tagList.toArray(msg.getTags()));

                logger.info("Message: "+msg.toString());

                messageList.add(msg);
            }

        } finally {
			// Release the connection.
			method.releaseConnection();
		}

        return messageList;
    }
}

