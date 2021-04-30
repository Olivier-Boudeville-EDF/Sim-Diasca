/**
 * 
 */
package common;

import java.util.ArrayList;
import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 * 
 * Abstract class only defining the base pattern that should follow any specialized communicator class.
 */
public abstract class BaseCommunicator {
	/**
	 * Defines how to communicate outputs to the user of the binding, i.e the (Erlang) core of the engine
	 * @param request_result
	 * @param request_type
	 */
	public abstract void send(ArrayList<?> request_result, String request_type);
	
	/**
	 * Defines which treatments, specific to the user of the binding (i.e the (Erlang) core of the engine),
	 * are to be performed on incoming messages.
	 * @param elements_msg
	 * @return message receive object
	 */
	public abstract MsgReceive filter(OtpErlangObject[] elements_msg) throws Exception;	
}
