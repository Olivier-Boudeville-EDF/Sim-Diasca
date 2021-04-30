/**
 * 
 */
package common;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.ericsson.otp.erlang.*;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 * 
 * Communicator class defining how this Java binding is to communicate with the (Erlang) core of the engine.
 * An instance of this class is intended to be *the unique communicator* (singleton) used within the 
 * corresponding Java interpreter to communicate with Erlang.
 */
public class ErlangCommunicator extends BaseCommunicator {
	
	private OtpErlangPid erlang_pid;
	private OtpMbox mbox;

	/**
	 * Constructor of a communicator that is specific to Erlang. 
	 * @param initiator_erlang_pid
	 * @param mbox
	 */
	public ErlangCommunicator(OtpErlangPid initiator_erlang_pid, OtpMbox mbox) {
		this.erlang_pid = initiator_erlang_pid;
		this.mbox = mbox;
	}
	
	/**
	 * Sends a message from java to the Erlang core.
	 * Here the 'send' operation is achieved by the 'cast' function implemented by the ErlPort library.
	 * 
	 * Moreover, in order to be able to perform selective receives in the Erlang world onto the messages originating
	 * from java, we add a flag to the beginning of the message that is to be sent (a header), which takes the form
	 * of an Erlang atom.
	 * 
	 * The structure of an **output message** is conventional defined as a 2-tuple of the 
	 * form **( message_headers, message_body )**.
	 * 
	 * The 'message_headers' part is itself a tuple of size 2 or 3: its first element is always 'java_message',
	 * while the remaining ones depend on the type of message:
	 * 
	 *   - for traces (TraceEmitter.java), the 2 additional headers are:
	 *     ( 'trace_emitted', String trace_type )
	 *     
	 *   - for exceptions (idem):
	 *     ( 'exception_raised', String exception_type )
	 *     
	 *   - for request results, the only additional header is:
	 *     'request_completed'
	 *     
	 * These additional headers must be the first members of the 'message' argument received here, received as strings.
	 * They will be turned into Erlang atoms (actually the ErlPort equivalent type) before sending.
	 * 
	 * The 'message_body' part of the final output message must be set inside a container stored in the last element of 
	 * the argument.
	 * 
	 * Considering all this, the 'message' argument is thus also expected to be a tuple of total size 3 or 4 
	 * (additional headers above + message_body).
	 */
	@Override
	public void send(ArrayList<?> request_result, String request_type) {
		// for a request_result completed:
		// request_result is message_body
		
		// for a request_result completed with error or trace:
		// the first element is parts of message_header and the last is the message_body (error message)
		OtpErlangTuple tuple_response;
		
		if(!(request_type == "request_completed")) {
			
			// init header tuple for response
			OtpErlangTuple tuple_header = new OtpErlangTuple(new OtpErlangObject[] 
					{new OtpErlangAtom("java_message"), new OtpErlangList(request_type)});
			
			// case of getPortSpecifications, request_result contain a list of map
			if(request_result.iterator().next() instanceof Map<?,?>) { 
				List<OtpErlangList> list_body = new ArrayList<>();
				
				// init body tuple for response
				for (Object object : request_result) {
					list_body.add(Convert.MaptoOtpErlangList((Map<Object, Object>) object));					
				}				
				tuple_response = getTupleResponse((ArrayList<?>) list_body, tuple_header);
			} else {
				// init body tuple for response
				tuple_response = getTupleResponse(request_result, tuple_header);				
			}
		} else { // case of trace or exception
			// init header tuple for response: "java_message" + (request_type + request_result[0])
			OtpErlangTuple tuple_h = new OtpErlangTuple(new OtpErlangObject[] 
					{new OtpErlangAtom(request_type), new OtpErlangAtom(request_result.get(0).toString())});
			OtpErlangTuple tuple_header = new OtpErlangTuple(new OtpErlangObject[] 
					{new OtpErlangAtom("java_message"), tuple_h});
			
			request_result.remove(0); // remove parts of message_header to request			
			
			// init body tuple for response
			tuple_response = getTupleResponse(request_result, tuple_header);						
		}	
		this.mbox.send(this.erlang_pid, tuple_response);
		this.mbox.close();
	}

	/**
	 * Create response tuple
	 * @param arraybody
	 * @param tuple_header
	 * @return OtpErlangTuple
	 */
	private OtpErlangTuple getTupleResponse(ArrayList<?> arraybody,OtpErlangTuple tuple_header) {
		OtpErlangObject[] array_body = (OtpErlangObject[]) arraybody.toArray(new OtpErlangObject[arraybody.size()]);
		OtpErlangTuple tuple_body = new OtpErlangTuple(new OtpErlangObject[] {new OtpErlangList(array_body) });
		return	new OtpErlangTuple(new OtpErlangObject[] {tuple_header, tuple_body});
	}

	/**
	 * Describes what filtering operations are to be done on messages coming from Erlang.
	 * @param elements_msg
	 * @return MsgReceive
	 */
	@Override
	public MsgReceive filter(OtpErlangObject[] elements_msg) throws Exception {		
		return new MsgReceive((OtpErlangAtom)elements_msg[1], (OtpErlangTuple)elements_msg[2]);		
	}
}
