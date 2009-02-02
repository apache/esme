/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.apache.esme.model;

/**
 *
 * @author dhague
 *
 */
public class Status {

    private String id;
    private String image;
    private String nickname;
    private String wholeName;

    public String toString()
    {
        return "id="+id+", nickname="+nickname+", wholeName="+wholeName+", image="+image;
    }

    /**
     * Get the value of wholeName
     *
     * @return the value of wholeName
     */
    public String getWholeName() {
        return wholeName;
    }

    /**
     * Set the value of wholeName
     *
     * @param wholeName new value of wholeName
     */
    public void setWholeName(String wholeName) {
        this.wholeName = wholeName;
    }

    /**
     * Get the value of id
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Set the value of id
     *
     * @param id new value of id
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * Get the value of image
     *
     * @return the value of image
     */
    public String getImage() {
        return image;
    }

    /**
     * Set the value of image
     *
     * @param image new value of image
     */
    public void setImage(String image) {
        this.image = image;
    }

    /**
     * Get the value of nickname
     *
     * @return the value of nickname
     */
    public String getNickname() {
        return nickname;
    }

    /**
     * Set the value of nickname
     *
     * @param nickname new value of nickname
     */
    public void setNickname(String nickname) {
        this.nickname = nickname;
    }
}
