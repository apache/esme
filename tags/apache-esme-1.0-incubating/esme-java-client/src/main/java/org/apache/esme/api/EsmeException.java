package org.apache.esme.api;

public class EsmeException extends RuntimeException {
	/**
	 * 
	 */
	private static final long serialVersionUID = -9145163776335704925L;
	
	private int httpStatus;
	
	public EsmeException(int httpStatus)
	{
		this.setHttpStatus(httpStatus);
	}

	private void setHttpStatus(int httpStatus) {
		this.httpStatus = httpStatus;
	}

	public int getHttpStatus() {
		return httpStatus;
	}
}
