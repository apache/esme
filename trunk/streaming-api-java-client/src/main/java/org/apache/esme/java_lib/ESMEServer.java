package org.apache.esme.java_lib;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIUtils;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.entity.BufferedHttpEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;

public class ESMEServer {
	
	private String protocol="",host="",path="",token ="";
	private String lastResponse="";
	private int port=-1;
	
	private boolean connected= false;
	
	public String getProtocol() {
		return protocol;
	}

	public void setProtocol(String protocol) {
		this.protocol = protocol;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public int getPort() {
		return port;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public HttpClient getHttpclient() {
		return httpclient;
	}

	public void setHttpclient(HttpClient httpclient) {
		this.httpclient = httpclient;
	}

	private HttpClient httpclient =  new DefaultHttpClient();

	public ESMEServer(String protocol, String host,int port, String path, String token, boolean autoconnect 
			) throws URISyntaxException {
		super();
		this.protocol = protocol;
		this.host = host;
		this.path = path;
		this.token = token;
		this.port = port;
		
		if (autoconnect==true) { this.connect();}
		

	}
	
	public void connect() {
		List<NameValuePair> qparams = new ArrayList<NameValuePair>();
		qparams.add(new BasicNameValuePair("token", this.token));
		try {
			URI uri = URIUtils.createURI(this.protocol, this.host, this.port, this.path+"/api2/session", 
				    URLEncodedUtils.format(qparams, "UTF-8"), null);
			HttpPost httppost= new HttpPost(uri);
			System.out.println("string: " + httppost.getURI());
			HttpResponse logonResponse= this.httpclient.execute(httppost);			
			System.err.println(logonResponse.getStatusLine().getStatusCode());
			System.err.println(logonResponse.getStatusLine().getReasonPhrase());
			System.err.println(logonResponse.getStatusLine().toString());
			if (logonResponse.getStatusLine().getStatusCode()==200) {
				this.connected=true;				
			} else {
				this.connected=false;				
			}
			HttpEntity entity = logonResponse.getEntity();
			if (entity != null) {
			    entity = new BufferedHttpEntity(entity);
			    this.lastResponse= EntityUtils.toString(entity);
			}
		} catch (URISyntaxException e) {
			
			e.printStackTrace();
		} catch (ClientProtocolException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void disconnect() {
		this.httpclient= null;
	}
	
	public String getLastReponse() {
		return this.lastResponse;
	}
	
	public boolean isConnected() {
		return this.connected;
	}
	
	public void postMessage(String message, String via, String pool, String realm, String metadata,String replyTo) {
		List<NameValuePair> qparams = new ArrayList<NameValuePair>();
		qparams.add(new BasicNameValuePair("message", message));
		if (!(via.equals(""))) {qparams.add(new BasicNameValuePair("via", via)); }
		if (!(pool.equals(""))) {qparams.add(new BasicNameValuePair("pool", pool)); }
		if (!(realm.equals(""))) {qparams.add(new BasicNameValuePair("realm", realm)); }
		if (!(metadata.equals(""))) {qparams.add(new BasicNameValuePair("metadata", metadata)); }
		
		try {
			URI uri = URIUtils.createURI(this.protocol, this.host, this.port, this.path+"/api2/user/messages", URLEncodedUtils.format(qparams, "UTF-8"), null);
			HttpPost httppost= new HttpPost(uri);
			System.out.println("string: " + httppost.getURI());
			HttpResponse logonResponse= this.httpclient.execute(httppost);			
			System.err.println(logonResponse.getStatusLine().getStatusCode());
			System.err.println(logonResponse.getStatusLine().getReasonPhrase());
			System.err.println(logonResponse.getStatusLine().toString());
			if (logonResponse.getStatusLine().getStatusCode()==200) {
				this.connected=true;				
			} else {
				this.connected=false;				
			}
			HttpEntity entity = logonResponse.getEntity();
			if (entity != null) {
			    entity = new BufferedHttpEntity(entity);
			    this.lastResponse= EntityUtils.toString(entity);
			}
		} catch (URISyntaxException e) {
			
			e.printStackTrace();
		} catch (ClientProtocolException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	

}
