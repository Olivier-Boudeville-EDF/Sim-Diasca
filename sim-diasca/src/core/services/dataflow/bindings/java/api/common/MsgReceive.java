/**
 * 
 */
package common;

import com.ericsson.otp.erlang.*;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 *
 * Class defining a MsgReceive with the receive message to erlang
 */
public class MsgReceive {
	private OtpErlangAtom msg_title;
	private OtpErlangObject msg_body;
	
	// Get / set
	public OtpErlangAtom getMsg_title() {
		return msg_title;
	}
	public void setMsg_title(OtpErlangAtom msg_atom) {
		this.msg_title = msg_atom;
	}

	public OtpErlangObject getMsg_body() {
		return msg_body;
	}
	public void setMsg_body(OtpErlangTuple msg_body) {
		this.msg_body = msg_body;
	}

	/**
	 * Constructor of receive message to erlang
	 * @param msg_atom
	 * @param msg_body:
	 *   List([Atom(b'vehicle_type_unit'), Atom(b'VehicleTypeUnit'), List([90, 111, 101]), 2020, 0.5, 1.0])
	 */
	public MsgReceive(OtpErlangAtom msg_atom, OtpErlangTuple msg_body) {
		super();
		this.msg_title = msg_atom;
		this.msg_body = msg_body;
	}
	
	/**
	 * Get parameter in message body for instantiate class
	 * @return table parameters
	 */
	public Object[] GetParamConstructor() throws OtpErlangException {		
		Object[] tabParam = null;
		OtpErlangList list_body = (OtpErlangList) this.msg_body;
		
		for (int i = 2; i < list_body.arity(); i++) { //take param after atoms "class name"
			if(list_body.elementAt(i) instanceof OtpErlangList) {
				tabParam[i-2] =  ((OtpErlangList)list_body.elementAt(i)).stringValue();
			} else if (list_body.elementAt(i) instanceof OtpErlangInt) {
				tabParam[i-2] =  ((OtpErlangInt)list_body.elementAt(i)).intValue();
			} else if (list_body.elementAt(i) instanceof OtpErlangDouble) {
				tabParam[i-2] =  ((OtpErlangDouble)list_body.elementAt(i)).doubleValue();
			} else if (list_body.elementAt(i) instanceof OtpErlangString) {
				tabParam[i-2] =  ((OtpErlangString)list_body.elementAt(i)).stringValue();
			}			
		}
		return tabParam;
	}
	
	/**
	 * Create Activation data object with message body 
	 * @return ActivationData
	 */
	public ActivationData getActivationData() {
		return new ActivationData(this.msg_body);
	}
	
	/**
	 * Get the class name for instantiation
	 * @return class name
	 */
	public String getClassName() {
		OtpErlangTuple tuple_msg_body = (OtpErlangTuple) this.msg_body;
		return ((OtpErlangString) tuple_msg_body.elementAt(1)).stringValue();
	}	
}

