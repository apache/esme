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
	private String text = "";
	private String[] tags = {};
	private String via = "Java API";
	/**
	 * @return
	 */
	public String[] getTags() {
		return tags;
	}

	/**
	 * @return
	 */
	public String getText() {
		return text;
	}

	/**
	 * @return
	 */
	public String getVia() {
		return via;
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
	public void setText(String string) {
		text = string;
	}

	/**
	 * @param string
	 */
	public void setVia(String string) {
		via = string;
	}

}
