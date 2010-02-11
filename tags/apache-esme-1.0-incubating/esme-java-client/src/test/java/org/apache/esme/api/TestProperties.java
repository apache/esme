/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.apache.esme.api;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;

/**
 *
 * @author dhague
 */
class TestProperties {
    static Properties properties = null;

    static public String getProperty(String propName)
    {
        if (properties == null)
        {
            properties = new Properties() ;
            URL url =  ClassLoader.getSystemResource("props/test.properties");
            try {
                properties.load(new FileInputStream(new File(url.getFile())));
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        return properties.getProperty(propName);
    }
}

