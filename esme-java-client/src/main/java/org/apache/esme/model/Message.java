/*
 * Created on 05-Oct-2008
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package org.apache.esme.model;

/**
 * @author I056593
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
