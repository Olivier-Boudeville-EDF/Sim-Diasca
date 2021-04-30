/**
 * 
 */
package common;

import java.util.ArrayList;
import java.util.Arrays;


/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 * 
 * Class defining how to send applicative traces (including errors and exceptions) outside of
 * the Java binding, directly in the coupling infrastructure.
 */
public class TraceEmitter {
	
	public static BaseCommunicator communicator;

	// Get / set
	public BaseCommunicator getCommunicator() {
		return communicator;
	}
	public void setCommunicator(BaseCommunicator communicator) {
		TraceEmitter.communicator = communicator;
	}

	/**
	 * Constructor that binds once for all the communicator (a singleton) to the instantiated trace emitted
	 */
	public TraceEmitter(BaseCommunicator communicator) {
		TraceEmitter.communicator = communicator;
	}
	
	/**
	 * Specialized trace sending API method, used for debug information.
	 * @param trace_message
	 */
	public void sendDebug(String trace_message) {
		
		this.sendGenericTrace("debug", trace_message);
	}	
	
	/**
	 * Specialized trace sending API method, used for basic traces.
	 * @param trace_message
	 */
	public void sendTrace(String trace_message) {
		
		this.sendGenericTrace("trace", trace_message);
	}	

	/**
	 * Specialized trace sending API method, used for more important notifications.
	 * @param trace_message
	 */
	public void sendInfo(String trace_message) {
		
		this.sendGenericTrace("info", trace_message);
	}
	
	/**
	 * Specialized trace sending API method, used for warning messages.
	 * @param trace_message
	 */
	public void sendWarning(String trace_message) {
		
		this.sendGenericTrace("warning", trace_message);
	}
	
	/**
	 * Specialized trace sending API method, used for reporting errors.
	 * @param trace_message
	 */
	public void sendError(String trace_message) {
		
		this.sendGenericTrace("error", trace_message);
	}
	
	/**
	 * Generic internal function used for sending traces to the external world.
	 * 
	 * Sends the trace message (a triplet) via the communicator. The trace
	 * message is characterized by the prepended 'trace_emitted' string.
	 * @param trace_type
	 * @param trace_message
	 */
	private void sendGenericTrace(String trace_type, String trace_message) {
		
		TraceEmitter.communicator.send(new ArrayList<>(Arrays.asList(trace_type, trace_message)), "trace_emitted");
	}

	/**
	 * Generic internal function used for redirecting caught exceptions towards the external world.
	 * 
	 * Sends the exception as a message (a triplet) via the communicator. The exception message is characterized 
	 * by the prepended 'exception_raised' string. The type of the exception is extracted from the exception object
	 * and formatted, in order to be separated from the representation.
	 * @param exp
	 */
	public void sendException(Exception exp) {		
		
		TraceEmitter.communicator.send(new ArrayList<>(Arrays.asList( exp.getMessage().toString(), 
				exp.getStackTrace().toString())), "exception_raised");
	}
}