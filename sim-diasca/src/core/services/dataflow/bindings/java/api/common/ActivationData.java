/**
 * 
 */
package common;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * @author J.Guillemineau - julien-externe.guillemineau@edf.fr
 * 
 * Create an activation data object from Erlang message
 */
public class ActivationData {
	
	private OtpErlangTuple simulation_state;
	private OtpErlangList input_ports_data;
	private OtpErlangList input_port_iterations_data;

	// Get / set
	public OtpErlangTuple getSimulation_state() {
		return simulation_state;
	}
	public void setSimulation_state(OtpErlangTuple simulation_state) {
		this.simulation_state = simulation_state;
	}
	
	public OtpErlangList getInput_ports_data() {
		return input_ports_data;
	}
	public void setInput_ports_data(OtpErlangList input_ports_data) {
		this.input_ports_data = input_ports_data;
	}
	
	public OtpErlangList getInput_port_iterations_data() {
		return input_port_iterations_data;
	}
	public void setInput_port_iterations_data(OtpErlangList input_port_iterations_data) {
		this.input_port_iterations_data = input_port_iterations_data;
	}
	
	/**
	 * Constructor of Activation data object
	 */
	public ActivationData(OtpErlangObject msg_body) {
		super();
		OtpErlangTuple tuple_msg_body = (OtpErlangTuple) msg_body;
		this.setSimulation_state((OtpErlangTuple) tuple_msg_body.elementAt(1));
		
		this.setInput_ports_data((OtpErlangList) tuple_msg_body.elementAt(2));
		this.setInput_port_iterations_data((OtpErlangList) tuple_msg_body.elementAt(3));
	}
}
