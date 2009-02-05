package org.apache.esme.api;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.apache.esme.api.EsmeRestApi;
import junit.framework.TestCase;
import org.apache.esme.model.Status;

public class RestTest extends TestCase {

    private static Logger logger = Logger.getLogger("org.apache.esme.api");
    private static String token = TestProperties.getProperty("esme-token");
    private static String esmeServer = TestProperties.getProperty("esme-server");

    public void testStatus() throws Exception
    {
        EsmeRestApi.debugMode = true;

        logger.log(Level.INFO, "Testing message status");

        EsmeRestApi esme = new EsmeRestApi(esmeServer);
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

        EsmeRestApi esme = new EsmeRestApi(esmeServer);

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

        EsmeRestApi esme = new EsmeRestApi(esmeServer);
        esme.login(token);

        Status status = esme.getStatus();
        assertNotNull(status);
        logger.log(Level.INFO, "API status is "+status);

        List messages = esme.getMessages();
        assertNotNull(messages);

        logger.info("Got "+messages.size()+" messages from server.");

        esme.logout();
    }

}
