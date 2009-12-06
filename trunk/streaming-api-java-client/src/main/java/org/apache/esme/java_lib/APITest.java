package org.apache.esme.java_lib;

import java.net.URISyntaxException;


public class APITest {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		try {
			ESMEServer s= new ESMEServer("http","localhost",8080,"/esme-server-0.3.0-SNAPSHOT","VPCABZLAPRZATETGXCPEU13W5FU2UIHR",true);
			System.out.println(s.isConnected());			
			System.out.println("l:" + s.getLastReponse());
			s.postMessage("hallo_ w/ #saü #sap #tag " , "", "", "", "", "");
			System.out.println("l:" + s.getLastReponse());
			s.disconnect();
		} catch (URISyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		

	}

}
