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

package org.apache.esme.model;

/**
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class Message {
//        <message id="1134" source="web" date="...">
//                <author image="..." name="..." id="9"/>
//                <body>
//                        Woohoo - got through the rest of the slides quite quickly. I can go home now.
//                </body>
//                <tags/>
//        </message>

    private String messageId;
	private String source = "Java API";
    private String date;

    private String authorId;
    private String authorName;
    private String authorImage;

    private String body = "";
	private String[] tags = {};

    public String toString()
    {
        StringBuffer tagsStr = new StringBuffer();
        tagsStr.append("[");
        for (int i=0; i<tags.length; i++)
        {
            if (i!=0) tagsStr.append(", ");
            tagsStr.append(tags[i]);
        }
        tagsStr.append("]");

        return "messageId="+messageId+", source="+source+", date="+date+", authorId="+
                authorId+", authorName="+authorName+", authorImage="+authorImage+", body="+
                body+", tags="+tagsStr.toString();
    }
    
	/**
	 * @return
	 */
	public String[] getTags() {
		return tags;
	}

	/**
	 * @return
	 */
	public String getBody() {
		return body;
	}

	/**
	 * @return
	 */
	public String getSource() {
		return source;
	}

	/**
	 * @param strings
	 */
	public void setTags(String[] strings) {
		tags = strings;
	}

	/**
	 * @param string
	 */
	public void setBody(String string) {
		body = string;
	}

	/**
	 * @param string
	 */
	public void setSource(String string) {
		source = string;
	}

        public String getAuthorId() {
        return authorId;
    }

    public void setAuthorId(String authorId) {
        this.authorId = authorId;
    }

    public String getAuthorImage() {
        return authorImage;
    }

    public void setAuthorImage(String authorImage) {
        this.authorImage = authorImage;
    }

    public String getAuthorName() {
        return authorName;
    }

    public void setAuthorName(String authorName) {
        this.authorName = authorName;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getMessageId() {
        return messageId;
    }

    public void setMessageId(String messageId) {
        this.messageId = messageId;
    }

}
